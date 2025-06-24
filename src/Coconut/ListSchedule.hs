-- Module      :  Coconut.ListSchedule
-- Copyright   :  (c) OCA 2021
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module contains functionality to schedule a Z CodeGraph by 
-- running a greedy list scheduler

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Coconut.ListSchedule where

import Coconut.BaseTypes
import Coconut.Utils.CGWrappers
import Coconut.Graph.DataFlow
import Coconut.Graph.ControlFlow
import Coconut.Graph.CodeGraph
import Coconut.Core.CoreISA
import Coconut.Core.ControlISA (compose,branch,doWhile)
import Coconut.Core.CoreHardware (CORE)
import Coconut.Core.MetaData
import Coconut.CodeGen
import Coconut.RegisterAllocator
import Coconut.Schedule
import Coconut.Graph.Dot
import Coconut.Simulator
import Coconut.HashedSchedule
-- import Data.ByteString hiding (foldr)
import qualified Data.ByteString.Char8 as BS

import Data.Bifunctor
import Data.Graph.Inductive.Graph hiding (Graph)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS (topsort)

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

-- | Simple list instruction scheduling algorithm.
--
-- See @listScheduleDFGraph@ for the core algorithm. This simply calls
-- @listScheduleCFGraph@ which in turn calls @listScheduleDFGraph@ in
-- topological order, which returns a list of ordered @Node@ that are given
-- a dispatch time according to their index in the list
listSchedule :: forall h . Hardware h => CodeGraph h -> MDMap h -> Schedule
listSchedule = listScheduleCFGraph

-- | List scheduling algorithm for basic blocks (i.e., data flow graphs)
--
-- Currently no real list scheduling algorithm is implemented, we obtain
-- a schedule by simply performing a topoligical sort (via FGLs @topsort@)
-- on the @DataFlowGraph@, making sure to put branch instructions last
--
-- NOTE this scheduling algorithm doesn't take into account data dependencies
-- for instructions that overwrite their arguments, and thus will yield an
-- invalid schedule for @DataFlowGraph@ that contain such instructions
listScheduleDFGraph :: forall h. (Hardware h, Show (RegType h))
  => MDMap h
  -> (String,DataFlowGraph h)
  -> (Schedule, Double)
listScheduleDFGraph metaData (label,dfg@(DataFlowGraph fglGraph dfIns dfOuts overwrites incr stage)) =
  let
    -- tests if a node is an instruction (NOT including InitMR's)
    isInstruction :: Node -> Bool
    isInstruction node =
      case match node fglGraph of
        (Just (_,_,InstructionNode el,_),_) ->
          case el of
            (Instruction _ _ _) -> True
            (Move _ _) -> True
            (Spill _) -> True
            (Despill _) -> True
            -- FIXME to include or not include initMR's in scheduling .. that is the question
            (InitMR _ _ ) -> True
            _ -> False
        (Just (_,_,BranchNode _ _ _,_),_) -> True
        _ -> False

    allInstructions :: [LNode (DFNode h)]
    allInstructions =
      let
        allInstrNodes = filter (isInstruction) $ topsort fglGraph
      in [ lNode | lNode <- labNodes fglGraph, (fst lNode) `elem` allInstrNodes ]

    instructionNodesList :: [Node]
    instructionNodesList = map fst $  instructionNodes dfg

    latencyHeightMap :: Map.Map Node Int
    latencyHeightMap = latencyHeight dfg
                        (map fst allInstructions)
                        (instructionLatency dfg metaData)

    instructionOverwriteDependencies :: Map.Map Node [Node]
    instructionOverwriteDependencies =
        Map.fromList
        $ map  
            (\(overwriteInstr,(predVR, succVR)) ->
               ( overwriteInstr
               , [ deps | deps <- suc fglGraph predVR, deps /= overwriteInstr] )
            )
        overwrites

    startCycle = 1

    startActive, startReady :: Set Node
    startActive = Set.empty
    startReady =
      Set.fromList $ filter (null . instructionProducers dfg) instructionNodesList

    startSchedule :: Schedule
    startSchedule = Map.empty
  in
    listScheduleDFGraphIterate startCycle startActive startReady latencyHeightMap
      (instructionOverwriteDependencies `debug` ("Overwrite dependencies: " ++ show instructionOverwriteDependencies)) dfg metaData startSchedule

-- | List schedule a @DataFlowGraph@ using forward list scheduling.
--   Iteratively selects the next instruction to dispatch in a particular cycle,
--   and exits when all instructions are scheduled
-- TODO: Deal with overwriting instructions: Add fake dependencies
-- Result register that's fed into an overwriting instruction
-- should have all its dependencies executed first before executing the
-- overwriting instruction itself
listScheduleDFGraphIterate :: (Hardware h, Show (RegType h))
  => Double
  -> Set Node
  -> Set Node
  -> Map.Map Node Int
  -> Map.Map Node [Node]
  -> DataFlowGraph h
  -> MDMap h
  -> Map.Map Node Double
  -> (Map.Map Node Double, Double)
listScheduleDFGraphIterate cycle active ready heights overwriteDeps df@(DataFlowGraph fglGraph dfIns dfOuts overwrites incr stage) mdMap oldSchedule =
  let
    _3 :: (a, b, c) -> c
    _3 (_, _, n) = n

    scheduledNodesSet :: Set Node
    scheduledNodesSet = Map.keysSet oldSchedule

    isReady :: Node -> Bool
    isReady n =
      Set.fromList (instructionProducers df n) `Set.isSubsetOf` scheduledNodesSet

    indexIntoLatencies n = case heights Map.!? n of
      Just lat -> lat
      Nothing -> error $ "Failed to find latency " ++ show n ++ " in " ++ show heights

    indexIntoSchedule o = case oldSchedule Map.!? o of
      Just dispatchTime -> dispatchTime
      Nothing -> error $ "Failed to find node " ++ show o ++ " in " ++ show oldSchedule

    updateReady :: Node -> (Set Node, Set Node) -> (Set Node, Set Node)
    updateReady o (activeSet, readySet) =
      let nodeInSchedule = o `Map.member` oldSchedule
          nodeDispatchTime =  oldSchedule Map.! o
          nodeIsOverwrite n = n `Set.member` Map.keysSet overwriteDeps
          potentialReady = instructionConsumers df o
      -- ∀ o ∈ Active where S(a) + λ(a) ≤ Cycle
      in
        if nodeInSchedule
            && nodeDispatchTime + fromIntegral (instructionLatency df mdMap o) <= cycle
        then ( Set.delete o activeSet -- Active ← Active - a
             -- ∀ b ∈ succs(a) where isReady(b)
             , foldr (\n s -> if isReady n then Set.insert n s else s) readySet
               $ filter (not . nodeIsOverwrite) potentialReady -- Ready ← Ready ∪ b
             )
        else (activeSet, readySet)

    updateOverwriteReady :: Set Node -> (Node, [Node]) -> Set Node -> Set Node
    updateOverwriteReady currentActive (overwriteInstr, predInstrs) currentReady =
      if Set.fromList predInstrs `Set.isSubsetOf` scheduledNodesSet
         && overwriteInstr `Set.notMember` scheduledNodesSet
         && Set.null (currentActive `Set.intersection` Set.fromList predInstrs)
      then Set.insert overwriteInstr currentReady
      else currentReady

    updateActive :: Set Node -> Set Node -> (Set Node, Set Node, Schedule)
    updateActive activeSet readySet =
      let
        -- ∃ a ∈ Ready
        prioSortedReadySet =
          Set.map
          (\n -> (heights Map.! n, instructionLatency df mdMap n, n))
          readySet
        -- and, ∀ b, a_priority ≥ b_priority
        nextDispatch = _3 $ Set.findMax prioSortedReadySet
        newReady = Set.delete nextDispatch readySet -- Ready ← Ready - a
        updatedSchedule = Map.insert nextDispatch cycle oldSchedule -- S(op) ← Cycle
        newActive = Set.insert nextDispatch activeSet -- Active ← Active ∪ a
      in
        if readySet == Set.empty
          then (activeSet, readySet, oldSchedule)
          else (newActive, newReady, updatedSchedule)

    (active', ready') = foldr updateReady (active, ready) (Set.toList active)
    updatedReadyOverwrites = foldr (updateOverwriteReady active') ready' $ Map.toList overwriteDeps
    (newActive, newReady, newSchedule) = updateActive active' updatedReadyOverwrites

  in
    if Set.union ready active == Set.empty
      then (oldSchedule, cycle)
      else listScheduleDFGraphIterate (cycle + 1)
           newActive
           newReady heights overwriteDeps df mdMap
           newSchedule

-- | List schedule the @ControlFlowGraph@.
--
-- Currently control flow has no influence on the inner data flow graph
-- scheduling algorithm so the code simply calls @listScheduleDFGraph@ on each
-- of the inner @DataFlowGraph@ and puts them together in topological order.
--
-- NOTE Topological sort is done by calling FGLs topsort function on the
-- controlFlowGraph
listScheduleCFGraph :: forall h . Hardware h => CodeGraph h -> MDMap h -> Schedule
listScheduleCFGraph cg metaData =
  let
    cfGraph = cgControlFlow cg
    dfGraphs = cgDataFlows cg

    sortedCFNodes :: [Node]
    sortedCFNodes = topsort $ controlFlowGraph cfGraph

    cfNodePositions :: [(Node,Int)]
    cfNodePositions = Prelude.zip sortedCFNodes [0..]

    onlyDFEdges :: [LEdge (CFEdge h)]
    onlyDFEdges = Prelude.filter (\(_,_,e) -> case e of
                             CFDataFlow _ -> True
                             _ -> False) $ labEdges $ controlFlowGraph cfGraph

    sortedCFEdges :: [LEdge (CFEdge h)]
    sortedCFEdges = List.sortBy edgeTopOrder onlyDFEdges

    edgeTopOrder :: LEdge (CFEdge h) -> LEdge (CFEdge h) -> Ordering
    edgeTopOrder (n0,_,_) (n1,_,_) =
      case (List.lookup n0 cfNodePositions,List.lookup n1 cfNodePositions) of
        (Just p1,Just p2) -> compare p1 p2
        _ -> error $ "edgeTopOrder has missing edge nodes: " ++ show n0 ++ ", " ++ show n1

    sortedDataFlows :: [(String,DataFlowGraph h)]
    sortedDataFlows = Prelude.concatMap (\(_,_,e) -> case e of
                                    CFDataFlow label -> case List.lookup label dfGraphs of
                                                          Just dfGraph -> [(label,dfGraph)]
                                                          Nothing -> error $ "Missing dfGraph: " ++ label
                                    _ -> []) sortedCFEdges

    scheduledDataFlows :: [(Schedule, Double)]
    scheduledDataFlows = map (listScheduleDFGraph metaData) sortedDataFlows

    finalSchedule = List.foldl' applyCycleOffset (Map.empty, 0) scheduledDataFlows
      where applyCycleOffset (inSchedule, inCycle) (curSchedule, curCycle) =
              ( Map.map (+ inCycle) curSchedule `Map.union` inSchedule
              , inCycle + curCycle)

   in fst finalSchedule
