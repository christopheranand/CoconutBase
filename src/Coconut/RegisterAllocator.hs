-- |
-- Module      :  Coconut.RegisterAllocator
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a functionality for performing register allocation

{-# LANGUAGE ScopedTypeVariables, TupleSections, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Coconut.RegisterAllocator where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Sequence (Seq,(<|),ViewR(..),viewr)
import qualified Data.Sequence as Sequence
import Data.Set (Set)
import qualified Data.Set    as Set
import qualified Data.List   as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe,catMaybes,fromJust)
import Data.Either (lefts,rights)
import Data.Function (on)
import Data.Bifunctor (bimap)
import Data.ByteString.Char8 (ByteString)
import Data.Ord (Down(..))

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS (topsort)
import Data.Graph.Inductive.Query.Dominators (dom)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Coconut.BaseTypes
    ( RegMap,
      SpillMap,
      Hardware(regColors, isConstantLoad, RegType, memRegType,
               allRegTypes),
      ScheduledEdge(..),
      ScheduledNode(..),
      DFEdge(DFEdge),
      DFNode(BranchNode, ResourceNode, InstructionNode),
      CFNode(..),
      DataFlowGraph(dataFlowOverwrites, dataFlowIncrement,
                    dataFlowGraph),
      ControlFlowGraph(ControlFlowGraph, controlFlowGraph),
      ScheduledGraph,
      CodeGraph(CodeGraph, cgMRTables, cgOutput, cgDataFlows,
                cgControlFlow, cgInput),
      Schedule,
      CFEdge(..),
      ResType(MemoryRes, RegisterRes, SpillRes),
      debug, EL (..))
import Coconut.Utils.CGWrappers (Unwrappable)
import Coconut.Utils.CGUtils
import Coconut.Graph.CodeGraph
import Coconut.Graph.ControlFlow
import Coconut.Graph.DataFlow
import Coconut.Core.Printer (InstructionFormat(TODO))
import Coconut.HashedSchedule (isConstLoad)

-- * Scheduled Graph

instance Show (ResType h) => Show (ScheduledNode h) where
  show (ScheduledNode instrN uses defs unmap gConst regPres) = "ScheduledNode " ++ show instrN ++ " "
                                          ++ show uses ++ " " ++ show defs
                                          ++ " " ++ show unmap ++ " "  ++ show gConst ++ " "  ++ show regPres

-- | Reorder certain edges so we don't have to cover redundant cases like
-- [BranchEQ,BranchNE] and [BranchNE,BranchEQ]
orderCFEdges :: [(CFEdge h,Node)] -> [(CFEdge h,Node)]
orderCFEdges =
  let
    cfEdgeOrd (e0,_) (e1,_) = case (e0,e1) of
      (CFDataFlow _,CFDataFlow _) -> EQ
      (CFDataFlow _,_) -> LT
      (CFCompose _ _,CFCompose _ _) -> EQ
      (CFCompose _ _,_) -> LT
      (CFJump _ _,CFJump _ _) -> EQ
      (CFJump _ _,_) -> LT
      (CFBranchNE _ _,CFBranchNE _ _) -> EQ
      (CFBranchNE _ _,_) -> LT
      (CFBranchEQ _ _,CFBranchEQ _ _) -> EQ
      (CFBranchEQ _ _,_) -> GT
      (CFModuloBody _,CFModuloBody _) -> EQ
      (CFModuloBody _,_) -> LT
  in List.sortBy cfEdgeOrd

-- | Generate a @ScheduledGraph@ from a @Schedule@ and a @CodeGraph@
buildScheduledGraph :: forall h. Hardware h
                    => Schedule
                    -> CodeGraph h
                    -> ScheduledGraph h
buildScheduledGraph schedule codeGraph =
  let
    (CodeGraph cfGraph dfGraphs cgIn cgOut mrTables cTable tags maxID dbgMap) = codeGraph

    firstCFNode = case match cgIn (controlFlowGraph cfGraph) of
      (Just (_,_,cfNode,_),cfGraph') -> cfNode
      (Nothing,_) -> error $ "buildScheduledGraph couldn't find cgOut in cfGraph: " ++ show cgIn

    firstRegs = case firstCFNode of
      (CFNode regs) -> regs
      (CFBranchNode _ regs) -> regs
      (CFJoinNode _) -> error "buildScheduledGraph ControlFlowGraph finished on CFJoinNode"

    finalCFNode = case match cgOut (controlFlowGraph cfGraph) of
      (Just (_,_,cfNode,_),cfGraph') -> cfNode
      (Nothing,_) -> error $ "buildScheduledGraph couldn't find cgOut in cfGraph: " ++ show cgOut

    finalRegs = case finalCFNode of
      (CFNode regs) -> regs
      (CFBranchNode _ regs) -> regs
      (CFJoinNode _) -> error "buildScheduledGraph ControlFlowGraph finished on CFJoinNode"
    -- NOTE we need to create a fake first/final instruction that declares all inputs/outputs or
    -- liveness analysis will overlook them
    fakeFirstNode =  ScheduledNode cgIn (map (nodeWithResTypeCG codeGraph . snd) firstRegs) [] cgIn False Nothing
    fakeFinalNode = ScheduledNode cgOut (map (nodeWithResTypeCG codeGraph . snd) finalRegs) [] cgOut False Nothing

    -- the set of dominators for each CFNode in the ControlFlowGraph
    cfDoms = dom (controlFlowGraph cfGraph) cgIn

    -- need to artificially float all initMR's to the top
    initMRs = map (\(_,n,_) -> n) $ cgMRTables codeGraph
    initMRToScheduledNode node = case matchNodeInCG codeGraph node of
      CGDataFlowNode (lbl,_) ->
        let
          dfGraph = fromJust $ List.lookup lbl dfGraphs
        in case context (dataFlowGraph dfGraph) node of
                        (_,_,_,adjDefs) ->
                          let
                            defs = map (\(_,n) -> (n,fromJust $ matchResType n dfGraph ))  adjDefs
                          in ScheduledNode node [] defs (node - dataFlowIncrement dfGraph) False Nothing
      _ -> error $ "initMRToScheduledNode given none initMR node: " ++ show node
    initMRScheduledNodes = map initMRToScheduledNode initMRs
    -- remove initMR's from schedule
    schedule0 = Map.filterWithKey (\k _ -> case matchNodeInCG codeGraph k of
                                      CGDataFlowNode (l,dfNode) -> not $ isInitMR dfNode
                                      _ -> True) schedule

    initMREdges =
      let
        iterateInitMRNodes (n0:n1:ns) = (n0,n1,SECompose) : iterateInitMRNodes (n1:ns)
        iterateInitMRNodes _ = []
        mrEdges = iterateInitMRNodes mrNodes
        mrNodes = (map snInstructionNode $ initMRScheduledNodes )
      in case mrEdges of
           [] -> case mrNodes of
                   [n] -> [(n,cgIn,SECompose)]
                   _ -> []
           _ -> mrEdges ++ [(case last mrEdges of (_,n,_) -> n,cgIn,SECompose)]

    -- build schedule graph WITHOUT fake instructions
    scheduledGraph0 :: ScheduledGraph h
    scheduledGraph0 =
      case cfGraphToScheduleGraph codeGraph schedule0 cfDoms fakeFinalNode [] [] (cgInput codeGraph) of
                        (lEdges,lNodes) -> mkGraph lNodes (List.nub lEdges)

    realFirstNode :: Node
    realFirstNode = case topsort scheduledGraph0 of
                      (n:_) -> n
                      [] -> error "buildScheduledGraph constructed empty graph"

    scheduledGraph = insEdge (cgIn,realFirstNode,SECompose)
                     $ insNode (cgIn,fakeFirstNode)
                     scheduledGraph0
    -- add initMR's
    scheduledGraphWithHeader =
      let
        withEdges = foldr insEdge withNodes initMREdges
        withNodes = foldr insNode scheduledGraph $ map (\s -> (snInstructionNode s,s)) initMRScheduledNodes
      in withEdges
  in scheduledGraphWithHeader

-- | Used by @buildScheduledGraph@ to iterate through a @ControlFlowGraph@ and
-- build the list of @LEdge@'s and @LNode@'s needed to create a @ScheduledGraph@
cfGraphToScheduleGraph :: Hardware h => CodeGraph h
                       -> Schedule
                       -> [(Node,[Node])]
                       -> ScheduledNode h
                       -> [LEdge ScheduledEdge]
                       -> [LNode (ScheduledNode h)]
                       -> Node
                       -> ([LEdge ScheduledEdge],[LNode (ScheduledNode h)])
cfGraphToScheduleGraph cg@(CodeGraph cfGraph dfGraphs cgIn cgOut mrTables cTable tags maxID dbgMap)
  schedule cfDoms finalNode lEdges lNodes node =
  -- match current node in the ControlFlowGraph
  case match node (controlFlowGraph cfGraph) of
    (Just (adjPrevs,_,cfNode,adjSuccs),cfGraph') ->
      --  pattern match on different combinations of CFNode and it's edges
      case cfNodeToScheduleGraph cg schedule cfDoms finalNode node cfNode adjSuccs of
        -- base case, no more nodes to traverse
        (lEdges',lNodes',[]) -> (lEdges' ++ lEdges,lNodes' ++ lNodes)
        -- recursive fold over all new nodes, accumulating lEdges and lNodes
        (lEdges',lNodes',ns) ->
          foldl (\(lEdges'',lNodes'') n' ->
                   cfGraphToScheduleGraph cg schedule cfDoms finalNode lEdges'' lNodes'' n')
                (lEdges' ++ lEdges,lNodes' ++ lNodes) ns
    (Nothing,_) -> error $ "cfGraphToScheduleGraph bad node lookup: " ++ show node


-- | Used by cfGraphToScheduleGraph@ to match on @ControlFlowGraph@ patterns and generate
-- @LEdge@'s and @LNode@'s to build a corresponding @ScheduledGraph@
cfNodeToScheduleGraph :: Hardware h => CodeGraph h
                      -> Schedule
                      -> [(Node,[Node])]
                      -> ScheduledNode h
                      -> Node
                      -> CFNode
                      -> [(CFEdge h, Node)]
                      -> ([LEdge ScheduledEdge], [LNode (ScheduledNode h)], [Node])
-- CFNode
cfNodeToScheduleGraph codeGraph schedule cfDoms finalNode node (CFNode dfNodes) adjSuccs =
      case orderCFEdges adjSuccs of
        -- data flow: add all insturction nodes and edges from on instruction to
        -- another in the schedule. Will create a completely linear graph
        [(CFDataFlow label,node')] ->
          let
            cgOut = cgOutput codeGraph
            dfGraph = fromJust $ List.lookup label (cgDataFlows codeGraph)
            schedList = if node' == cgOut
                          then dfGraphToSchedList (label,dfGraph) (Just cgOut) schedule
                          else dfGraphToSchedList (label,dfGraph) Nothing schedule
            lEdges' = schedListToLEdges schedList
            lNodes' = schedListToLNodes dfGraph cgOut finalNode schedList
          in (lEdges',lNodes',[node'])
        -- Add an edge from the last node in the schedule from the previous
        -- block to the first node in the schedule of the composed block
        [(CFCompose (labelA,labelB) ties,node')] ->
          let
            dfGraphA = case List.lookup labelA $ cgDataFlows codeGraph of
                         Just dfGraph -> dfGraph
                         Nothing -> error $ "cfNodeToScheduleGraph given missing label: " ++ show labelA
            dfGraphB = case List.lookup labelB $ cgDataFlows codeGraph of
                         Just dfGraph -> dfGraph
                         Nothing -> error $ "cfNodeToScheduleGraph given missing label: " ++ show labelB
            -- the node that's first in the schedule of DataFlowGraph with lblA
            lastNodeA = case dfGraphToSchedList (labelA,dfGraphA) Nothing schedule of
                          [] -> error $ "Block " ++ labelA ++ "is empty"
                          xs -> last xs
            -- the node that's first in the schedule of DataFlowGraph with lblB
            firstNodeB = case dfGraphToSchedList (labelB,dfGraphB) Nothing schedule of
                          [] -> error $ "Block " ++ labelB ++ "is empty"
                          xs -> head xs
            -- Create edge lastNodeA -> firstNodeB
            lEdges' = [(lastNodeA,firstNodeB,SECompose)]
            -- All nodes are added when CFDataFlow edge's are processed, hence [] is sufficent
            lNodes' = []
          in (lEdges',lNodes',[node'])
        -- Base Cases
        [] -> ([],[],[])
        ns -> error $ "Bad control flow pattern in cfNodeToScheduleGraph on nodes: "
                    ++ show node ++ " -> " ++ show (map snd ns)
-- CFBranchNode
cfNodeToScheduleGraph codeGraph schedule cfDoms finalNode node (CFBranchNode bNode dfNodes) adjSuccs =
      case orderCFEdges adjSuccs of
        -- branch: create two edges from the branch node (i.e. bNode) to the
        -- two nodes the branched blocks that come first in the schedule. Returns
        -- the two control flow nodes of the branched blocks to be processed
        [(CFJump (lblA,lblB) ties,node')] ->
          let
            cgOut = cgOutput codeGraph
            dfGraphB = case List.lookup lblB $ cgDataFlows codeGraph of
                         Just dfGraph -> dfGraph
                         Nothing -> error $ "cfNodeToScheduleGraph given missing label: " ++ show lblB
            -- the node that's first in the schedule of DataFlowGraph with lblB
            firstNodeB = case dfGraphToSchedList (lblB,dfGraphB) Nothing schedule of
                          [] -> error $ "Block " ++ lblB ++ "is empty"
                          xs -> head xs
            -- Create edges jump node -> firstNodeB
            lEdges' = [(snd bNode,firstNodeB,SECompose)]
            -- Need to add node for jump instruction here, because it's not in
            -- the dataflow graph
            lNodes' = []
            -- OR if node' is the final node
            lEdgesF = [(snd bNode,cgOut,SECompose)]
            lNodesF = [(cgOut,finalNode)]
            -- next nodes to iterate over
            ns' = if node' `elem` (fromJust $ List.lookup node cfDoms)
                      then [] -- if node' is a dominator of node, stop or we'll end in an infinite loop
                      else [node']
          in if node' == cgOut
                then (lEdgesF,lNodesF,ns')
                else (lEdges',lNodes',ns')
        -- branch: create two edges from the branch node (i.e. bNode) to the
        -- two nodes the branched blocks that come first in the schedule. Returns
        -- the two control flow nodes of the branched blocks to be processed
        [(CFBranchNE lblB tiesB,nodeB),(CFBranchEQ lblA tiesA,nodeA)] ->
          let
            cgOut = cgOutput codeGraph
            dfGraphA = case List.lookup lblA $ cgDataFlows codeGraph of
                         Just dfGraph -> dfGraph
                         Nothing -> error $ "cfNodeToScheduleGraph given missing label: " ++ show lblA
            dfGraphB = case List.lookup lblB $ cgDataFlows codeGraph of
                         Just dfGraph -> dfGraph
                         Nothing -> error $ "cfNodeToScheduleGraph given missing label: " ++ show lblB
            -- the node that's first in the schedule of DataFlowGraph with lblA
            firstNodeA = case dfGraphToSchedList (lblA,dfGraphA) Nothing schedule of
                          [] -> error $ "Block " ++ lblA ++ "is empty"
                          xs -> head xs
            -- the node that's first in the schedule of DataFlowGraph with lblB
            firstNodeB = case dfGraphToSchedList (lblB,dfGraphB) Nothing schedule of
                          [] -> error $ "Block " ++ lblB ++ "is empty"
                          xs -> head xs
            -- Create edges bNode -> firstNodeA and bNode -> firstNodeB
            lEdges' = case (nodeA==cgOut,nodeB==cgOut) of
              (True,True) -> [(snd bNode,cgOut,SECompose)]
              (True,False) -> [(snd bNode,cgOut,SEBranchEQ),(snd bNode,firstNodeB,SEBranchNE)]
              (False,True) -> [(snd bNode,cgOut,SEBranchNE),(snd bNode,firstNodeA,SEBranchEQ)]
              (False,False) -> [(snd bNode,firstNodeA,SEBranchEQ),(snd bNode,firstNodeB,SEBranchNE)]
            -- All nodes are added when CFDataFlow edge's are processed, hence [] is sufficent
            lNodes' = if nodeA == cgOut || nodeB == cgOut
                         then [(cgOut,finalNode)]
                         else []
            -- next nodes to iterate over
            ns' = [ n | n <- [nodeA,nodeB], not (n `elem` (fromJust $ List.lookup node cfDoms))]
          in (lEdges',lNodes',ns')
        ns -> error $ "Bad control flow pattern in cfNodeToScheduleGraph on nodes: "
                    ++ show node ++ " -> " ++ show (map snd ns)
--- cfJoinNode
cfNodeToScheduleGraph codeGraph schedule cfDoms finalNode node (CFJoinNode nodePairs) adjSuccs =
       case orderCFEdges adjSuccs of
         [(CFDataFlow label,node')] ->
          let
            cgOut = cgOutput codeGraph
            dfGraph = fromJust $ List.lookup label (cgDataFlows codeGraph)
            schedList = if node' == cgOut
                          then dfGraphToSchedList (label,dfGraph) (Just cgOut) schedule
                          else dfGraphToSchedList (label,dfGraph) Nothing schedule
            lEdges' = schedListToLEdges schedList
            lNodes' = schedListToLNodes dfGraph cgOut finalNode schedList
          in (lEdges',lNodes',[node'])
         -- FIXME ending with a CFJoinNode allowed?
         -- Base Cases
         [] -> ([],[],[])
         ns -> error $ "Bad control flow pattern in cfNodeToScheduleGraph on nodes: "
                     ++ show node ++ " -> " ++ show (map snd ns)

-- | Returns a list of every @Node@ in a given @DataFlowGraph@ sorted by @Schedule@
-- NOTE this reinserts constant loads directly before their use (which assumes all constant loads are
-- rematerialized)
dfGraphToSchedList :: (Hardware h)
                   => (String, DataFlowGraph h)
                   -> Maybe Node
                   ->  Schedule
                   -> [Node]
dfGraphToSchedList (label,dfGraph) mFinalNode schedule  =
  let
    schedList0 = map fst $ List.sortOn snd
                         $ Map.toList
                         $ Map.filterWithKey (\n t -> isElemOfDF dfGraph n && not (isConstantLoad dfGraph n))
                                             schedule

    isBranch dfGraph n =
      case match n (dataFlowGraph dfGraph) of
        (Just (_,_,nodeLbl,_),_) -> case nodeLbl of
                                      BranchNode {} -> True
                                           -- `debug` ("matched branch in graph: " ++ label)
                                      _ -> False
        (Nothing,_) -> False

    reinsertConstants [] = []
    -- NOTE we group pairs of instructions with 3 inputs for Z hardwares grouping functionality
    -- probably want to get rid of this on other architectures (just delete this (n0:n1:ns) pattern match)
    -- FIXME this will create a bug if an instruction overwrites its contant input
    reinsertConstants (n0:n1:ns) =
      let
        has3Inputs n = length (pre (dataFlowGraph dfGraph) n) == 3
        constantProducers0 = filter (isConstantLoad dfGraph) $ instructionProducers dfGraph n0
        constantProducers1 = filter (isConstantLoad dfGraph) $ instructionProducers dfGraph n1
      in if has3Inputs n0 && has3Inputs n1
            then constantProducers0 ++ constantProducers1 ++ [n0,n1] ++ reinsertConstants ns
            else constantProducers0 ++ [n0] ++ reinsertConstants (n1:ns)
    reinsertConstants (n:ns) =
      let
        constantProducers = filter (isConstantLoad dfGraph) $ instructionProducers dfGraph n
      in constantProducers ++ [n] ++ reinsertConstants ns

    schedList1 = filter (not . isBranch dfGraph) $ reinsertConstants schedList0

    -- if there's a dangling constant load at the end (i.e, its not the "producer" of any instr)
    -- then reinsertConstants will miss it, but it might be needed in the next block, so we'll
    -- add them to the end (but before the branch instruction if there is one)'
    allConstLoads = map fst $ Map.toList $ Map.filterWithKey (\n t -> isConstantLoad dfGraph n) schedule
    missingConstLoads = filter (`notElem` schedList1) allConstLoads
    branchInstrs = filter (isBranch dfGraph) schedList0

    finalNode = case mFinalNode of
                  Just n -> [n]
                  Nothing -> []
    schedListFinal = schedList1 ++ missingConstLoads ++ branchInstrs
                     ++ finalNode
  in if not $ null missingConstLoads
        then schedListFinal `debug` ("dfGraphToSchedList " ++ label ++"\n" ++ show schedListFinal)
        else schedListFinal

-- | Returns a list of every @Node@ in a given @DataFlowGraph@ sorted by @Schedule@
-- NOTE assumes constant loads have been scheduled
dfGraphToSchedListKeepConstants :: (Hardware h)
                                => (String, DataFlowGraph h )
                                -> Maybe Node
                                -> Schedule
                                -> [Node]
dfGraphToSchedListKeepConstants (label,dfGraph) mFinalNode schedule =
  let
    schedList0 = map fst $ List.sortOn snd
                         $ Map.toList
                         $ Map.filterWithKey (\n t -> isElemOfDF dfGraph n) schedule
    finalNode = case mFinalNode of
                  Just n -> [n]
                  Nothing -> []
  in schedList0 ++ finalNode

-- | Given a list of @Node@ assumed to be sorted in terms of some @Schedule@, returns the
-- list of @LEdge@'s needed to build the corresponding @ScheduledGraph@
schedListToLEdges :: [Node] -> [LEdge ScheduledEdge]
schedListToLEdges (n0:n1:ns) = (n0,n1,SECompose) : schedListToLEdges (n1:ns)
schedListToLEdges _          = []

-- | Returns the @ResType@ of a @Node@ in a given @DataFlowGraph@
-- Throws an error if the node is not a @ResourceNode@
nodeWithResType :: DataFlowGraph h -> Node -> (Node,ResType h)
nodeWithResType dfGraph node =
  case matchDFNode node dfGraph of
    (Just (ResourceNode res)) -> (node,res)
    (Just (InstructionNode _)) -> error $ "nodeWithResType given InstructionNode: " ++ show node
    (Just (BranchNode _ _ _)) -> error $ "nodeWithResType given BranchNode: " ++ show node
    Nothing -> error $ "nodeWithResType given bad node: " ++ show node

-- | Returns the @ResType@ of a @Node@ in a given @CodeGraph@
-- Throws an error if the node is not a @ResourceNode@
nodeWithResTypeCG :: CodeGraph h -> Node -> (Node,ResType h)
nodeWithResTypeCG cg node =
  case matchNodeInCG cg node of
    (CGControlFlowNode cfNode) -> error $ "nodeWithResTypeCG given cf node: " ++ show node
    (CGDataFlowNode (label,dfNode)) -> case dfNode of
            (ResourceNode res) -> (node,res)
            (InstructionNode _) -> error $ "nodeWithResTypeCG given InstructionNode: " ++ show node
            (BranchNode _ _ _) -> error $ "nodeWithResTypeCG given BranchNode: " ++ show node
    CGNotFound -> error $ "nodeWithResTypeCG given bad node: " ++ show node

-- | Takes a list of @Node@ and converts them into the list of @LNode (ScheduledNode h)@
-- needed to build the @ScheduleGraph@ corresponding to the given @DataFlowGraph@
schedListToLNodes :: DataFlowGraph h -> Node -> ScheduledNode h -> [Node] -> [LNode (ScheduledNode h)]
schedListToLNodes dfGraph cgOut finalNode (n:ns)
  | n == cgOut = (n,finalNode) : schedListToLNodes dfGraph cgOut finalNode ns
  | otherwise = case match n (dataFlowGraph dfGraph) of
      (Just (prevs,_,(InstructionNode _),succs),_) ->
        let
          dfEdgeCmp (DFEdge idx0,_) (DFEdge idx1,_) = compare idx0 idx1
          prevs' = map (nodeWithResType dfGraph . snd) $ List.sortBy dfEdgeCmp prevs
          succs' = map (nodeWithResType dfGraph . snd) $ List.sortBy dfEdgeCmp succs
        in (n,(ScheduledNode n prevs' succs' (n - dataFlowIncrement dfGraph) False Nothing))
                : schedListToLNodes dfGraph cgOut finalNode ns
      (Just (prevs,_,(BranchNode _ _ _),succs),_) ->
        let
          dfEdgeCmp (DFEdge idx0,_) (DFEdge idx1,_) = compare idx0 idx1
          prevs' = map (nodeWithResType dfGraph . snd) $ List.sortBy dfEdgeCmp prevs
          succs' = map (nodeWithResType dfGraph . snd) $ List.sortBy dfEdgeCmp succs
        -- FIXME a BranchNode should always be the last instruction in a schedule, but should this
        -- be enforced here?
        in (n,(ScheduledNode n prevs' succs' (n - dataFlowIncrement dfGraph) False Nothing))
                : schedListToLNodes dfGraph cgOut finalNode ns
      (Just (_,_,(ResourceNode _),_),_) ->
        error $ "schedListToLNodes list contains ResourceNode: " ++ show n
      (Nothing,_) -> error $ "schedListToLNodes list contains bad node: " ++ show n
schedListToLNodes dfGraph _ _ [] = []

-- * Liveness Sets

-- | Represents the "liveness" of registers at a given instruction going in
-- (i.e. @livenessIn@) and out (i.e. @livenessOut@) of an instruction. Each
-- IntMap is a map from an instruction @Node@ to a set of register @Node@ that
-- are "alive"
data LivenessSets = LivenessSets { livenessIn :: IntMap IntSet
                                 , livenessOut :: IntMap IntSet
                                 }
  deriving (Eq)
instance Show LivenessSets where
  show (LivenessSets liveIn liveOut) = "LiveIn:\n" ++ unlines (map show $ IM.toList liveIn)
                                     ++ "\nLiveOut:\n" ++ unlines (map show $ IM.toList liveOut)

-- | Left-biased union of @LivenessSets@ (simply does on @Data.IM.union@ on
-- in and out sets)
livenessUnion :: LivenessSets -> LivenessSets -> LivenessSets
livenessUnion (LivenessSets in0 out0) (LivenessSets in1 out1) =
  LivenessSets (IM.union in0 in1) (IM.union out0 out1)

-- FIXME
-- LivenessSets should obey the following "DataFlow Constraints"
-- create tests to make sure these constraints are satisfied by the end of liveness analysis
-- use[n] \subseteq in[n]
-- out[n] - def[n] \subseteq in[n]
-- for n' \in succ[n], in[n'] \subseteq out[n]

-- | Run a liveness analysis (create sets of registers that are "alive" at any
-- given instruction in a schedule). Essentially implements the following
-- iterative algorithm
--
-- ```
-- for all instructions at node n, in[n] = empty, out[n] = empty
-- repeat until no change in "in" and "out"
--  for all n
--     out[n] = Union_{n' elem succ[n]}  in[n']
--     in[n] = use[n] `union` (out[n] - def[n])
-- ```
livenessSets :: ScheduledGraph h -> IntMap Node -> LivenessSets
livenessSets scheduledGraph coalesceMap =
  let

    allNodes :: [Node]
    allNodes = reverse $ topsort scheduledGraph

    initSet :: IntMap IntSet
    initSet = IM.fromList $ zip allNodes (repeat IS.empty)

    initLivenessSets :: LivenessSets
    initLivenessSets = LivenessSets initSet initSet

    -- recursively apply liveness passes until the output converges
    runLivenessAnalysis :: LivenessSets -> LivenessSets
    runLivenessAnalysis liveSets
      | liveSets == liveSets' = liveSets -- `debug` (show liveSets)
      | otherwise = runLivenessAnalysis liveSets' -- `debug` (show liveSets')
      where
        liveSets' = livenessPass liveSets scheduledGraph coalesceMap allNodes

  in runLivenessAnalysis initLivenessSets

-- a single liveness pass
livenessPass :: LivenessSets -> ScheduledGraph h -> IntMap Node -> [Node] -> LivenessSets
livenessPass liveSets scheduledGraph coalesceMap [] = liveSets
livenessPass liveSets scheduledGraph coalesceMap (n:ns) = case context scheduledGraph n of
      (adjPrevs,node,ScheduledNode _ uses defs _ _ _,adjSuccs) ->
        let
          coalesceLookup :: Node -> Node
          coalesceLookup n = case IM.lookup n coalesceMap of
                              Just n' -> n'
                              Nothing -> n -- error $ "Node missing in coalesceMap: "  ++ show n
          -- coalesce all reg nodes to properly perform liveness analysis
          uses' = map (coalesceLookup . fst) uses
          defs' = map (coalesceLookup . fst) defs
          liveIn = livenessIn liveSets
          liveOut = livenessOut liveSets
          -- perform dataflow equations
          outN = foldl IS.union IS.empty $ map (\(_,n) -> fromJust $ IM.lookup n liveIn) adjSuccs
          inN = (IS.fromList uses') `IS.union`
                (outN `IS.difference` (IS.fromList defs'))
          liveIn' = IM.insert node inN liveIn
          liveOut' = IM.insert node outN liveOut
        in livenessPass (LivenessSets liveIn' liveOut') scheduledGraph coalesceMap ns

insertRegPressureSchedGraph :: forall h. Hardware h
  => ScheduledGraph h -> InterferenceGraph h -> LivenessSets -> ScheduledGraph h
insertRegPressureSchedGraph schedGraph interGraph liveSets =
  let
    liveIn = livenessIn liveSets
    insertRegPress (prevs,node,sNode,sucs) =
      let
        press = regPress node
      in (prevs,node,sNode { snRegPressure = Just press },sucs)

    regPress node = map (\regType -> (show regType, length
                                                    $ filter (\i -> nodeIsRegType regType $ lookupRegNode i)
                                                    $ IS.toList $ lookupLiveSet node))
                    (allRegTypes @h)

    lookupLiveSet node = case IM.lookup node liveIn of
                           Just intSet -> intSet
                           Nothing -> error $ "insertRegPressureSchedGraph missing node in liveset: "
                                      ++ show node

    nodeIsRegType :: RegType h -> RegNode h -> Bool
    nodeIsRegType regType (RegNode resType) = case resType of
                                                (RegisterRes regType') -> regType == regType'
                                                (MemoryRes _) -> regType == memRegType
                                                _ -> False

    lookupRegNode node = case match node interGraph of
                           (Just (_,_,regNode,_),_) -> regNode
                           _ -> error $ "insertRegPressureSchedGraph missing node in interGraph: "
                                ++ show node
   in gmap insertRegPress schedGraph

-- * Interference Graph

-- | An InterferenceGraph is an fgl graph used to identify register interference
--   patterns when register allocating. It is a type alias for @Gr (RegNode h) ()@
type InterferenceGraph h = Gr (RegNode h) ()

-- | Generate an interference graph for all registers in a given @CodeGraph@
-- with a given @Schedule@. Uses @livenessSets@ to compute the liveness of registers
genInterferenceGraph :: forall h. Hardware h => CodeGraph h -> ScheduledGraph h -> (IntMap Node,LivenessSets,InterferenceGraph h)
genInterferenceGraph cg@(CodeGraph cfGraph dfGraphs cgIn cgOut mrTables cTable tags maxID dbgMap) scheduledGraph =
  let
    liveSets :: LivenessSets
    liveSets = livenessSets scheduledGraph coalesceMap

    coalesceMap :: IntMap Node
    coalesceMap = registerCoalesceMap cg

    allLiveSets :: [IntSet]
    allLiveSets = (map snd $ IM.toList liveIns) ++ (map snd $ IM.toList liveOuts)

    coalesceLookup :: Node -> Node
    coalesceLookup n = case IM.lookup n coalesceMap of
                         Just n' -> n'
                         -- FIXME is it correct to just return n if not found in coalesceMap?
                         Nothing -> n -- error $ "bad node lookup in coalesce map for node: " ++ show n

    coalescedLiveSets :: [IntSet]
    coalescedLiveSets = allLiveSets
    -- coalescedLiveSets = map (IS.map coalesceLookup) allLiveSets

    liveIns,liveOuts :: IntMap IntSet
    (liveIns,liveOuts) = (livenessIn liveSets,livenessOut liveSets)

    intSetToPairs :: IntSet -> [(Int,Int)]
    intSetToPairs intSet = pairs $ IS.toList intSet

    pairs :: [a] -> [(a,a)]
    pairs xs = [(x,y) | (x:ys) <- List.tails xs, y <- ys]

    allEdges :: [(Int,Int)]
    allEdges = map NE.head . NE.group . NE.sort . NE.fromList .
               map (\(a,b) -> if a > b then (b,a) else (a,b)) $ allEdges0
    allEdges0 = concatMap intSetToPairs coalescedLiveSets

    allLEdges :: [LEdge ()]
    allLEdges = map (\(n0,n1) -> (n0,n1,())) allEdges

    allNodes :: [Int]
    allNodes = List.nub $ concatMap IS.toList allLiveSets
    -- allNodes = List.nub $ concatMap (\(x,y) -> [x,y]) allEdges

    lookupResType :: Node -> ResType h
    lookupResType n = case matchNodeInCG cg n of
      (CGDataFlowNode (_,dfNode)) ->
        case dfNode of
          (ResourceNode resType) -> resType
          (InstructionNode instr) -> error
            $ "genInterferenceGraph.lookupResType found instruction on node : "  ++ show n
          (BranchNode _ _ _) -> error
            $ "genInterferenceGraph.lookupResType found branch on node : "  ++ show n
      (CGControlFlowNode _) -> error $ "genInterferenceGraph.lookupResType found CFNode" ++ show n
      CGNotFound -> error $ "genInterferenceGraph.lookupResType bad node lookup" ++ show n

    allLNodes :: [LNode (RegNode h)]
    allLNodes = map (\n -> (n,RegNode $ lookupResType n)) allNodes

  in (coalesceMap,liveSets,mkGraph allLNodes allLEdges)

-- | Returns all the node ties (input/output pairs between basic blocks) in a
-- ControlFlowGraph. Ties stored in @CFEdge@ value constructors like
-- @CFCompose@, @CFBranchEQ@, etc
cfAllRegisterTies :: ControlFlowGraph h -> [(Node,Node)]
cfAllRegisterTies (ControlFlowGraph cfGraph cfIn cfOut) =
  let
    allCFEdges = map (\(_,_,cfEdge) -> cfEdge) $ labEdges cfGraph
    cfEdgeToTies cfEdge = case cfEdge of
                            CFCompose _ ties -> ties
                            CFBranchEQ _ ties -> ties
                            CFBranchNE _ ties -> ties
                            CFJump _ ties -> ties
                            _ -> []
  in List.nub $ concatMap cfEdgeToTies allCFEdges

-- TODO LUCAS when regCGAllocate returns node it fails on, that node will correspond
-- to multiple nodes, this map will tell you which nodes
-- find all of the nodes that map to the node it fails on

-- find those nodes in the largest scheduledChunks
-- then find that chunk in the dot graph (say for tan)

-- | Returns a map of register coalesce's (i.e., maps each register node to a
-- parent register node). This is different that just register ties in that it
-- coalesces chains of register ties to one parent (i.e. if you have ties
-- (n0,n1) and (n1,n2) both n1 and n2 will map to its parent node n0)
registerCoalesceMap :: CodeGraph h -> IntMap Node
registerCoalesceMap cg@(CodeGraph cfGraph dfGraphs cgIn cgOut mrTables cTable tags maxID dbgMap) =
  let
    -- register are tied together either in CFEdge's or instruction overwrites
    allTies = allRegTies ++ allOverwrites ++ allInitMRTies0
      `debug` ("\nallInitMRTies: " ++ show allInitMRTies0 ++ "\n")
    allRegTies = cfAllRegisterTies cfGraph
    allOverwrites = map snd $ concatMap (dataFlowOverwrites . snd)  dfGraphs

    allInitMRTies0 = concatMap (\(mrLabel,_,_) -> let
                                   mrNodes = [ n | (lbl,n) <- allInitMRNodes, lbl == mrLabel]
                                   minMR = minimum mrNodes
                                   in map (\n -> (n,minMR)) mrNodes)
                     mrTables

    allInitMRNodes = concatMap (\(_,dfGraph) -> concatMap (initToMRNode dfGraph)
                                                          $ initMRNodes dfGraph) dfGraphs

    initToMRNode dfGraph (n,dfNode) =
      case match n (dataFlowGraph dfGraph) of
        (Just (_,_,_,adjDefs),_) -> map (\(_,n') -> (initMRLabel (n,dfNode),n')) adjDefs
        (Nothing,_) -> error $ "initToMRNode given bad node" ++ show n

    initMRLabel (n,dfNode) = case dfNode of
      InstructionNode (InitMR lbl _) -> lbl
      _ -> error $ "initMRLabel given none initMR node " ++ show n

    -- reorder all ties so that all pairs are in order and then all ties are
    -- sorted by first node in pair (also remove redundant ties like (5,5))
    ordTies :: [(Node,Node)]
    ordTies = List.sortOn fst $ concatMap (\(n0,n1) -> case compare n0 n1 of
                                              EQ -> []
                                              LT -> [(n0,n1)]
                                              GT -> [(n1,n0)]) allTies
    -- group all tied nodes together into a list of sets by iterating through a
    -- sorted list of pairs, and adding pairs to a set if either nodes already
    -- exist in one, and creating a new set otherwise
    groupTies :: [IntSet] -> [(Node,Node)] -> [IntSet]
    groupTies sets ((n0,n1):ties)  = groupTies (addTieToSets (n0,n1) sets) ties
    groupTies sets []  = sets

    addTieToSets :: (Node,Node) -> [IntSet] -> [IntSet]
    addTieToSets (n0,n1) (s:ss)
      | n0 `IS.member` s || n1 `IS.member` s = IS.insert n1 (IS.insert n0 s) : ss
      | otherwise = s : addTieToSets (n0,n1) ss
    addTieToSets (n0,n1) [] = [IS.fromList [n0,n1]]

    -- merge all of the groupTies (i.e. if a Node from one set is inside another, the sets
    -- should be merged)
    mergeGroups :: [IntSet] -> [IntSet]
    mergeGroups (g:gs) =
      let
        overlappingSets = filter (\g0 -> g0 `IS.intersection` g /= IS.empty) gs
        g' = foldr IS.union g overlappingSets
        gs' = filter (\g0 -> g0 `IS.intersection` g == IS.empty) gs
      in g' : mergeGroups gs'
    mergeGroups [] = []

    -- given a list of sets of register ties, for each set pick the minimum
    -- element of each sets then create a map that maps each element to that
    -- minimum element
    setsToCoalesceMap [] = IM.empty
    setsToCoalesceMap (s:ss) = let
      k = IS.findMin s
      in  (foldr (\x -> IM.insert x k) IM.empty (IS.toList s))
          `IM.union` setsToCoalesceMap ss

  in setsToCoalesceMap (mergeGroups (groupTies [] ordTies))

-- * Register Allocation

-- | A RegNode is the node label type used in an @InterferenceGraph@, its a simple newtype wrapper
--   around @ResType h@
newtype RegNode h = RegNode (ResType h)

instance Show (RegNode h) where
  show (RegNode _) = ""


-- | Register allocate a @CodeGraph@ using @chaitinBriggsAllocate@. If it fails
-- will return a @Left@ value with a list of nodes that failed (one node per
-- @RegType@). If it succeeds will return a @RegMap@ that maps every register
-- node in the @CodeGraph@ to a register (i.e. @ByteString@)
regAllocateCG :: forall h. Hardware h
  => Schedule
  -> CodeGraph h
  -> [(String,ByteString)]
  -> Either ([(RegType h,Node,RegMap h)],LivenessSets) (RegMap h,SpillMap,ScheduledGraph h)
regAllocateCG schedule codeGraph inputRegs =
  let
    cfGraph :: ControlFlowGraph h
    cfGraph = cgControlFlow codeGraph

    cfIn :: CFNode
    cfIn = case matchCFNode (cgInput codeGraph) cfGraph of
             Just cfNode -> cfNode
             Nothing -> error "CodeGraph missing cgIn in ControlFlowGraph"

    inpNodes :: [(String,Node)]
    inpNodes = case cfIn of
                 CFNode inps -> inps
                 _ -> error "cgInput is not a CFNode"

    -- nodes (already remapped to their parent coalesced node) that have fixed input registers
    fixedNodes :: [(Node,ByteString)]
    fixedNodes = [(\ (n, r)
                    -> case IM.lookup n regCoalesceMap of
                                Just n' -> (n', r)
                                Nothing -> (n, r))
                  (n, r) |
                  (s0, r) <- inputRegs, (s1, n) <- inpNodes, s0 == s1]

    scheduledGraph :: ScheduledGraph h
    scheduledGraph = buildScheduledGraph schedule codeGraph

    -- regCoalesceMap maps nodes to their parent nodes (i.e., the node
    -- represented in the interferenceGraph)
    regCoalesceMap :: IntMap Node
    interferenceGraph :: InterferenceGraph h
    liveSets :: LivenessSets
    (regCoalesceMap,liveSets,interferenceGraph) = genInterferenceGraph codeGraph scheduledGraph

    nodeIsRegType :: RegType h -> RegNode h -> Bool
    nodeIsRegType regType (RegNode resType) = case resType of
                                                (RegisterRes regType') -> regType == regType'
                                                (MemoryRes _) -> regType == memRegType
                                                _ -> False

    nodeIsSpill :: RegNode h -> Bool
    nodeIsSpill (RegNode resType) = case resType of
                                      SpillRes -> True
                                      _ -> False

    spillNodes = nodes $ labfilter nodeIsSpill interferenceGraph
    spillMap0 = Map.fromList $ zip spillNodes [0..]
                 `debug` ("\nspillNodes: " ++ show spillNodes ++ "\n")
    spillMap = Map.union spillMap0 $
              Map.fromList $
              concatMap (\(n0,n) -> case Map.lookup n spillMap0 of
                                       Just offset -> [(n0, offset)]
                                       Nothing -> []
                      ) $ IM.toList regCoalesceMap

    interGraphsByReg :: [(RegType h,InterferenceGraph h)]
    interGraphsByReg = map (\regType -> (regType
                                        ,labfilter (nodeIsRegType regType) interferenceGraph))
                           (allRegTypes @h)

    coloringResults :: [Either (RegType h,Node,RegMap h) (RegMap h)]
    coloringResults = {-# SCC "coloringResults" #-}
                       map (\(r,g) ->
                             chaitinBriggsAllocate (regColors r) (r,g) fixedNodes) interGraphsByReg

    failedAllocations :: [(RegType h,Node,RegMap h)]
    failedAllocations = lefts coloringResults

    combinedRegMaps :: RegMap h
    combinedRegMaps = foldl Map.union Map.empty $ rights coloringResults

    missingRegMaps = IM.map (\n -> case Map.lookup n combinedRegMaps of
                                    Just n' -> n'
                                    Nothing -> error $ "failed to lookup node: " ++ show n
                                               ++ "\nregCoalesceMap: " ++ show regCoalesceMap
                                               ++ "\ncombinedRegMaps: " ++ show combinedRegMaps
                            )
                     $ IM.filter (`notElem` spillNodes)regCoalesceMap

    allRegMaps = Map.fromList (IM.toList missingRegMaps) `Map.union` combinedRegMaps
  in case failedAllocations of
       [] ->
         let
           (regMapC,schedGraphC) =
             allocateGlobalConstants codeGraph schedule allRegMaps scheduledGraph
         in Right (regMapC,spillMap,schedGraphC)
           -- Right (allRegMaps,spillMap,scheduledGraph)
       ns -> Left (ns,liveSets)

allocateGlobalConstants :: forall h . Hardware h
  => CodeGraph h
  -> Schedule
  -> RegMap h
  -> ScheduledGraph h
  -> (RegMap h,ScheduledGraph h)
allocateGlobalConstants cg schedule regMap schedGraph =
  let
    availableColours = concat [ map (\b -> (rt, b)) (regColors rt) | rt <- allRegTypes @h ]
    regNodeFilter _ = True
  in allocateGlobalConstants0 cg schedule regMap schedGraph regNodeFilter availableColours
     `debug` ("availableColors: " ++ show availableColours)

allocateGlobalConstants0 :: forall h . Hardware h
  => CodeGraph h
  -> Schedule
  -> RegMap h
  -> ScheduledGraph h
  -> (Node -> Bool)
  -> [(RegType h,ByteString)]
  -> (RegMap h,ScheduledGraph h)
allocateGlobalConstants0 cg schedule regMap schedGraph regNodeFilter availableColours =
  let
    allocatedRegs = map snd $ Map.toList regMap
    allocatedNodes = Map.keys regMap
    -- availableColours = concat [ map (\b -> (rt, b)) (regColors rt) | rt <- allRegTypes @h ]
    leftOverRegs = filter (`notElem` allocatedRegs) availableColours

    -- zip all constants values (represented as [Int]) with the "un"-instruction
    -- Node, the instruction DFNode, and the register Node respectively
    allConstLoads =
      map (\(n,dfNode,dfGraph,n') ->
                case dfNode of
                  InstructionNode (Instruction imms _ _)
                    -> (imms,(n,dfNode,dfGraph,n'))
                  _ -> error $ "node filtered by isConstantLoad is not instruction: " ++ show n
              ) allConstantLoads0
    allConstantLoads0 = concatMap (\(_,dfGraph)
            ->
            map (\(n,dfNode) -> (n,dfNode,dfGraph,head $ suc (dataFlowGraph dfGraph) n))
            $ filter (\(n,_) -> isConstantLoad dfGraph n
                             && isInScheduledGraph n
                             && all regNodeFilter (suc (dataFlowGraph dfGraph) n)
                             && head (suc (dataFlowGraph dfGraph) n) `notElem` allOverwrites
                     )
            $ labNodes
            $ dataFlowGraph dfGraph)
            $ cgDataFlows cg

    allOverwrites = concatMap ((\(n0,n1) -> [n0,n1]) . snd)
                        (concatMap (dataFlowOverwrites . snd)  $ cgDataFlows cg)

    isInScheduledGraph n =
        case match n schedGraph of
          (Nothing,_) -> False
          _ -> True

    -- assign length leftOverRegs most used constants (which are identified by
    -- [Int]) to new registers
    constRegAssigns :: [(([Int] {- constant value -},[Node] {- corresponding constant load instrs -})
                        ,(RegType h,ByteString) {- coloring with regtype -})]
    constRegAssigns =
      concatMap
        (\rt ->
          let
            leftOverRegs' = filter (\(rt',_) -> rt == rt') leftOverRegs
            allConstLoads' = filter (\(_,(_,_,_,n)) ->
                                              fst (regMapLookup n regMap) == rt
                                            ) allConstLoads
          in allocateGlobalConstantsByRegType cg leftOverRegs' allConstLoads')
        (allRegTypes @h)

    -- update RegMap with new constReg assignments
    lookupRegNodeFromLoadNode n =
      case List.lookup n $ map (\(_,(cN,_,_,rN)) -> (cN,rN)) allConstLoads of
        Just rN -> rN
        Nothing -> error $ "failed to lookup node in allConstLoads: " ++ show n
    regMapNew = foldr (\(n,color) rMap -> Map.insert n color rMap) regMap
              $ concatMap (\((_,nodes),color)
                           -> map ((\n -> (n,color)) . lookupRegNodeFromLoadNode) nodes
              ) constRegAssigns

    -- use the [Node]'s in constRegAssigns to remove constant loads from ScheduledGraph'
    -- then add a block to the top of the scheduled graph of global constant loads
    removeLocalConstLoads :: [Node] -> ScheduledGraph h -> ScheduledGraph h
    removeLocalConstLoads (constLoad:loads) schedGraph0 =
        case match constLoad schedGraph0 of
          (Just (prevs,_,sNode,[(_,nextN)]),_) ->
            let
              prevNs = map snd prevs
              schedGraph1 = delNode constLoad
                          $ deleteAndReAttachEdgesP prevs constLoad nextN -- delEdges (map (,constLoad) prevNs)
                          $ delEdge (constLoad,nextN)
                          schedGraph0
            in removeLocalConstLoads loads schedGraph1
            -- case (prevs,nexts) of
            --   ([(_,prev)],[(_,next)]) -> let
            --      schedGraph1 = delNode constLoad
            --                  $ delEdge (prev,constLoad)
            --                  $ delEdge (constLoad,next)
            --                    schedGraph0
            --    in removeLocalConstLoads loads schedGraph1
          (Just _,_) -> error $ "constant load has bad structure in scheduledGraph: " ++ show constLoad
          (Nothing,_) -> error $ "failed to lookup constant load in removeLocalConstLoads: " ++ show constLoad
    removeLocalConstLoads [] schedGraph0 = schedGraph0

    deleteAndReAttachEdgesP ((se,prevN):prevs) n nextN graph0 = let
      graph1 = insEdge (prevN,nextN,se)
             $ delEdge (prevN,n) graph0
      in deleteAndReAttachEdgesP prevs n nextN graph1
    deleteAndReAttachEdgesP [] n nextN graph0 = graph0

    -- reinsert constant loads as global constant block
    fstNode = head $ topsort schedGraph
    sndNode = case match fstNode schedGraph of
                (Just (_,_,_,[(_,next)]),_) -> next
                _ -> error "first node doesn't have second node (or is branch) in scheduleGraph"

    constScheduledNodes = map (\((_,nodes),_) -> let
                                 node = head nodes
                                 lNode = fromJust $ lab schedGraph node
                                 in (node,lNode { snGlobalConst = True })) constRegAssigns
    insertGlobalConstLoads cnodes schedGraph0 = let
      edges = let cs = fstNode : (map fst cnodes ++ [sndNode])
              in zipWith (\n0 n1 -> (n0, n1, SECompose)) cs (tail cs)
      in insEdges edges
         $ insNodes constScheduledNodes schedGraph0

    schedGraphNew = delEdge (fstNode,sndNode)
                  $ insertGlobalConstLoads constScheduledNodes
                  $ removeLocalConstLoads (concatMap (snd . fst) constRegAssigns) schedGraph

    -- update register registerCoalesceMap
    -- regCoalesceMapNew = let
    --   constNodes = map (\((_,nodes),_) ->
    --                       case nodes of
    --                         (n:ns) -> (n,ns) -- first node will be parent node in regCoalesceMap
    --                         _ -> error "constReg has empty list of nodes"
    --                       ) constRegAssigns
    --   in updateCoalesceMap constNodes regCoalesceMap

    -- updateCoalesceMap ((n,ns):nodes) cMap = let
    --     cMap' = foldr (`IM.insert` n) cMap (n:ns)
    --     in updateCoalesceMap nodes cMap'
    -- updateCoalesceMap [] cMap = cMap

  in case (leftOverRegs,allConstLoads) of
          ([],_) -> (regMap,schedGraph)
          (_,[]) ->(regMap,schedGraph)
          _ -> (regMapNew,schedGraphNew)

-- | This function allocates register colors to constant values to use as global
-- constant loads, given a list of register candidates and all constants loads that
-- ARE ALREADY FILTERED TO BE THE SAME REGISTER TYPE.
-- It prioritizes allocating constant loads that occur most frequently
allocateGlobalConstantsByRegType :: Hardware h
                                 => CodeGraph h
                                 -> [(RegType h,ByteString)]
                                 -> [([Int],(Node,DFNode h,DataFlowGraph h,Node))]
                                 -> [(([Int] -- the constant value
                                     ,[Node]) -- the nodes that correspond to the constant load instructions
                                     ,(RegType h,ByteString)) -- the resulting coloring
                                    ]
allocateGlobalConstantsByRegType cg leftOverRegs allConstLoads =
  let
    numLeftOversRegs = length leftOverRegs
     -- get list of constants used sorted from most to least
    sortedConsts = List.sortOn (Down . snd)
                  $ map (\grp -> ((fst $ head grp,map (\(imm,(n,_,_,_)) -> n) grp)
                                 , length grp))
                  $ groupConstVals allConstLoads
    groupConstVals [] = []
    groupConstVals ((imm,n):ns) =
      ((imm,n) : [ (imm0,n0) | (imm0,n0) <- ns, imm0 == imm])
                               : groupConstVals (filter (\(imm0,_) -> imm0 /= imm) ns)

    -- zip sortedConsts with leftOverRegs
    candidateConstants = zip (map fst sortedConsts) leftOverRegs
  in candidateConstants
    `debug` ("allocateGlobalConstants leftOverRegs: " ++ show leftOverRegs)

-- | Colors a subgraph @InterferenceGraph@ for a given @RegType@ using the
-- chaitin-briggs coloring algorithm. Returns an @Either@ value, @Left Node@ if
-- coloring fails on the returned @Node@, or @Right (RegMap h)@ if it succeeds
chaitinBriggsAllocate :: Hardware h
                      => [ByteString]
                      -> (RegType h,InterferenceGraph h)
                      -> [(Node,ByteString)]
                      -> Either (RegType h,Node,RegMap h) (RegMap h)
chaitinBriggsAllocate colors (regType,interferenceGraph) fixedNodes =
  let
    initRegMap = Map.empty
  in chaitinBriggsAllocate0 colors (regType,interferenceGraph) fixedNodes initRegMap

chaitinBriggsAllocate0 :: Hardware h
                      => [ByteString]
                      -> (RegType h,InterferenceGraph h)
                      -> [(Node,ByteString)]
                      -> RegMap h
                      -> Either (RegType h,Node,RegMap h) (RegMap h)
chaitinBriggsAllocate0 colors (regType,interferenceGraph) fixedNodes initRegMap0 =
  let
    numColors = length colors
    -- only concerned with nodes in the current interferenceGraph (i.e., of the same regType)
    fixedNodes' = filter (\(n,_) -> case match n interferenceGraph of
                             (Just _,_) -> True
                             _ -> False) fixedNodes
    -- pop fixed nodes (which are globally unavailable) nodes from interference graph
    initNodes = List.nub $ Map.toList (Map.map snd initRegMap0) ++ fixedNodes'
    interGraphNF = foldr (\(n,_) g -> case match n g of
                                       (_,g') -> g') interferenceGraph initNodes
    -- combine the initial regMap with fixed nodes
    initRegMap = initRegMap0 `Map.union`
                Map.fromList (map (\(n,b) -> (n,(regType,b))) fixedNodes')
    initColors = map (\c -> (c,length $ filter (==c) $ map snd initNodes)) colors
    -- While exists vertices with < numColors neighbours in graph
    --     pop a vertex and put it on the stack
    -- If graph is non-empty
    --     pick a vertex n (via a heuristic)
    --     pop it from the graph and put it on stack
    -- Repeat until graph is empty
    stack = chaitinBriggsGenStack numColors interGraphNF []
    -- Pop vertices off the stack and color with the lowest color not used by a
    -- neighbour
    -- If a node cannot be colored, returns Nothing and marks for spilling
    generatedColors = chaitinBriggsGenColors initColors regType initRegMap interferenceGraph stack
  in generatedColors

-- | Using a stack generated by @chaitinBriggsGenStack@, attempts to color an
-- interference graph by iteratively popping nodes of the stack, coloring with
-- the lowest color not in use by a neighbour. Returns an @Either@ value, @Left
-- Node@ if the coloring failed (with the @Node@ it failed on), or @Right
-- RegMap@ if the coloring succeeded
chaitinBriggsGenColors :: Hardware h
                       => [(ByteString,Int)]
                       -> RegType h
                       -> RegMap h
                       -> InterferenceGraph h
                       -> [Context (RegNode h) ()]
                       -> Either (RegType h,Node,RegMap h) (RegMap h)
chaitinBriggsGenColors colors regType regMap iGraph [] =
  let
    leftOverColors = [ b | (b,n) <- colors, n == 0 ]
  in Right regMap
     -- `debug` ("Allocated " ++ show regType ++ " with leftover regs " ++ show leftOverColors)
chaitinBriggsGenColors colors regType regMap iGraph (context:stack') =
  let
    (_,node,_,_) = context
    (prevs,succs) = case match node iGraph of
                      (Just (p,_,_,s),_) -> (p,s)
                      _ -> error $ "chaitinBriggsGenColors stack given node not in iGraph: " ++ show node
    neighbourColors = concatMap (\(_,n) -> case Map.lookup n regMap of
                                             Just (_,c) -> [c]
                                             Nothing -> []) (prevs ++ succs)
    candidateColors = filter (\(c,_) -> not (c `elem` neighbourColors)) colors
    -- (color,cnt) =  List.minimumBy (\(_,n0) (_,n1) -> compare n0 n1) candidateColors
    (color,cnt) =  List.maximumBy (\(_,n0) (_,n1) -> compare n0 n1) candidateColors
    regMap' = Map.insert node (regType,color) regMap
    colors' = (color,cnt+1) : (filter (\(c,_) -> c /= color) colors)
  in case candidateColors of
       [] -> Left (regType,node,regMap)
       _  -> chaitinBriggsGenColors colors' regType regMap' iGraph stack'

-- | Pops all the nodes off a graph using a combination of @popVertices@ and
-- @heuristicPopVertex@ (when the latter fails) and returns a stack of nodes (as
-- an fgl @Context@) used for coloring with the Chaitin-Briggs algorithm
chaitinBriggsGenStack :: Int
                      -> InterferenceGraph h
                      -> [Context (RegNode h) ()]
                      -> [Context (RegNode h) ()]
chaitinBriggsGenStack numColors graph stack
  | isEmpty graph' = stack'
  | otherwise = case heuristicPopVertex graph' of
                  (vertex,graph'') -> chaitinBriggsGenStack numColors graph'' (vertex:stack')
  where
    (stack',graph') = popVertices numColors graph stack

-- FIXME update the heuristic used to pop a vertex (possibly implement something with scheduler)
-- | Used to pop a vertex from a given graph after @popVertices@ fails to return
-- an empty graph (i.e., all the nodes left in the graph have degree > numColors)
heuristicPopVertex :: forall h . InterferenceGraph h -> (Context (RegNode h) (),InterferenceGraph h)
heuristicPopVertex graph =
  let
     nodes = map fst $ labNodes graph
     lEdges = labEdges graph
     nodeCounts = map (\n -> (n,length $ filter (\(n0,n1,_) -> n==n0 || n==n1) lEdges)) nodes
     -- pick node with the largest amount of edges
     n = case reverse $ List.sortOn snd nodeCounts of
     -- n = case List.sortOn snd nodeCounts of
           ((n,_):_) -> n
           _ -> error "heuristicPopVertex given empty graph"
  in case match n graph of
       (Just c,graph') -> (c,graph')
       _ -> error "heuristicPopVertex bad match"

-- | Runs popVertex in succession until it fails to return a node (i.e., there
-- is no node with less than numColors neighbors)
popVertices :: forall h . Int
            -> InterferenceGraph h
            -> [Context (RegNode h) ()]
            -> ([Context (RegNode h) ()],InterferenceGraph h)
popVertices numColors graph stack =
  let
    mVertex = popVertex numColors graph
  in case mVertex of
       Just (context,graph') -> popVertices numColors  graph' (context:stack)
       Nothing -> (stack,graph)

-- | Tries to "pop" a single node with less than numColors neighbors from the an
-- interference graph. If it fails to find such a node it returns nothing, if it
-- succeeds it returns the nodes @Context@ and the remaining @InterferenceGraph@
-- with node (and referenced edges) removed
popVertex :: forall h . Int
          -> InterferenceGraph h
          -> Maybe (Context (RegNode h) (),InterferenceGraph h)
popVertex numColors interferenceGraph =
  let
    candidates :: [LNode (RegNode h)]
    candidates = labNodes $ gfiltermap contextHasDegree interferenceGraph
    contextHasDegree context = case context of
      (prevs,_,_,succs) -> if length (prevs ++ succs) < numColors
                              then Just context
                              else Nothing
    -- sort candidates from most to least number of edges in the interferenceGraph
    sortedCandidates = reverse
                       $ List.sortOn snd
                       $ map (\(n,_) -> let
                               numEdges = length
                                 $ filter (\(n0,n1,_) -> n==n0 || n==n1)
                                 $ labEdges interferenceGraph
                             in (n,numEdges)) candidates
  in case candidates of
       [] -> Nothing
       ((n,_):ns) -> case match n interferenceGraph of
                       (Just context,graph') -> Just (context,graph')
                       (Nothing,_) -> error $ "bad match on node: " ++ show n

-- | List schedule nodes that were given the same schedule to optimize for
-- register allocation, by post sorting chunks given the same schedule by
-- the number of edges they have in the inteference graph
listScheduleForRegPressure :: forall h . DataFlowGraph h
                           -> Schedule
                           -> IntMap Node
                           -> InterferenceGraph h
                           -> [Node]
                           -> [Node]
listScheduleForRegPressure dfGraph schedule coalesceMap interGraph scheduledNodes =
  let
    fglGraph = dataFlowGraph dfGraph
    cmpScheduleTime n0 n1 =
      case (Map.lookup n0 schedule,Map.lookup n1 schedule) of
        (Just t0,Just t1) -> t0 == t1
        _ -> False

    -- group nodes that have the same schedule time
    -- NOTE assumes scheduledNodes are already in order
    groupNodes [] = []
    groupNodes (n:ns) = (n : takeWhile (cmpScheduleTime n) ns)
                      : (groupNodes $ dropWhile (cmpScheduleTime n) ns)

    scheduledChunks = groupNodes scheduledNodes

    cLookup n = case IM.lookup n coalesceMap of
                  Just n' -> n'
                  Nothing -> n
    -- for every instruction node, return the node with its incoming and
    -- outgoing regs (already coalesced) respectively
    nodeAdjacencies n =
      case match n fglGraph of
        (Just (prevs,_,_,succs),_) -> (n,map (cLookup . snd) prevs,map (cLookup . snd) succs)
        (Nothing,_) -> error $ "nodeInterferences given bad node " ++ show n
    -- number of edges a node has in the interference graph
    numInterEdges n =
      case match n interGraph of
        (Just (prevs,_,_,succs),_) -> length $ prevs ++ succs
        (Nothing,_) -> error $ "numInterEdges given bad node " ++ show n
    -- for every instruction node, return the node with its maximum interference
    -- edges, consuming and producing respectively
    maximum0 = foldr max 0
    scheduledChunks' = map (\ns ->
                              map (\n ->
                                     case nodeAdjacencies n of
                                       (n,cons,prods) -> (n
                                                         ,maximum0 $ map numInterEdges cons
                                                         ,maximum0 $ map numInterEdges prods)
                          ) ns) scheduledChunks
    -- push instructions that consume a reg with alot of interference up and
    -- instructions that produce a reg with alot of interferences down
    rescheduleChunk ns =
      let
        prods = List.sortOn snd $ concatMap (\(n,c,p) -> if p >= c then [(n,p)] else []) ns
        cons = reverse $ List.sortOn snd $ concatMap (\(n,c,p) -> if c > p then [(n,c)] else []) ns
      in map fst cons ++ map fst prods

  in (concat $ map rescheduleChunk scheduledChunks')
     `debug` ("largest scheduled chunk: "++(show $ maximum0 $ map length scheduledChunks))


-- | List schedule nodes that were given the same schedule to optimize for
-- register allocation, by post sorting chunks given the same schedule to
-- push instructions that are "final consumers" of a register up
listScheduleForRegPressure0 :: forall h . DataFlowGraph h
                           -> Schedule
                           -> IntMap Node
                           -> InterferenceGraph h
                           -> [Node]
                           -> [Node]
listScheduleForRegPressure0 dfGraph schedule coalesceMap interGraph scheduledNodes =
  let
    fglGraph = dataFlowGraph dfGraph
    allNodes = map fst $ labNodes fglGraph
    cmpScheduleTime n0 n1 =
      case (Map.lookup n0 schedule,Map.lookup n1 schedule) of
        (Just t0,Just t1) -> t0 == t1
        _ -> False

    -- group nodes that have the same schedule time
    -- NOTE assumes scheduledNodes are already in order
    groupNodes [] = []
    groupNodes (n:ns) = (n : takeWhile (cmpScheduleTime n) ns)
                      : (groupNodes $ dropWhile (cmpScheduleTime n) ns)

    scheduledChunks = groupNodes scheduledNodes

    cLookup n = case IM.lookup n coalesceMap of
                  Just n' -> n'
                  Nothing -> n

    isFinalConsumer n =
      let
        consumedRegs = map cLookup $ pre fglGraph n
        numConsumes = length $ filter (isFinalConsumer' n) consumedRegs
      in numConsumes

    isFinalConsumer' n r0 =
      let
        allInstrConsumers = concatMap (\n0 -> suc fglGraph n0)
                            $ filter (\n0 -> cLookup n0 == r0) allNodes
        nTime = case Map.lookup n schedule of
                  Just t -> t
                  Nothing -> error $ "isFinalConsumer' given node missing from schedule "++show n
        allInstrTimes = map (\n0 -> fromJust $ Map.lookup n0 schedule) allInstrConsumers
      in and [ nTime >= n0Time | n0Time <- allInstrTimes ]

    -- reschedule chunks to push instructions with more final consumes up
    rescheduleChunk ns =
      let
        finalConsumes = map (\n -> (n,isFinalConsumer n)) ns
       in map fst $ reverse $ List.sortOn snd finalConsumes
  in concat $ map rescheduleChunk scheduledChunks
     `debug` ("scheduledChunks: " ++ show scheduledChunks)

-- | An `ActiveInterval` wraps an interval
--   denoting start and end times, but with an Ord instance
--   sorting by increasing end point.
--   It also carries information of the register allocated to it
--   and the register node.
data ActiveInterval h = ActiveInterval { intervalTime :: (Int, Int)
                                       , intervalRegNum :: ByteString
                                       , intervalRegNode :: Node
                                       }

instance Eq (ActiveInterval h) where
  act0 == act1 = intervalTime act0 == intervalTime act1

instance Ord (ActiveInterval h) where
  ActiveInterval (s0, e0) _ _ <= ActiveInterval (s1, e1) _ _ =
    (e0 <= e1) || (s0 <= s1)

-- | Register allocate a @CodeGraph@ using a linear scan algorithm.
regAllocateLinearScanCG :: forall h. Hardware h
  => Schedule
  -> CodeGraph h
  -> [(String, ByteString)]
  -> Either ([(RegType h, Node)],LivenessSets) (RegMap h, SpillMap, ScheduledGraph h)
regAllocateLinearScanCG schedule codeGraph inputRegs =
  let
    cfGraph :: ControlFlowGraph h
    cfGraph = cgControlFlow codeGraph

    cfIn :: CFNode
    cfIn = case matchCFNode (cgInput codeGraph) cfGraph of
             Just cfNode -> cfNode
             Nothing -> error "CodeGraph missing cgIn in ControlFlowGraph"

    inpNodes :: [(String,Node)]
    inpNodes = case cfIn of
                 CFNode inps -> inps
                 _ -> error "cgInput is not a CFNode"

    coalesceMap :: IntMap Node
    coalesceMap = registerCoalesceMap codeGraph

    schedGraph :: ScheduledGraph h
    schedGraph = buildScheduledGraph schedule codeGraph

    liveSets :: LivenessSets
    liveSets = livenessSets schedGraph coalesceMap

    intervalMap :: Map (Int,Int) Node
    intervalMap = liveIntervals liveSets schedGraph

    initActiveIntervals :: Set (ActiveInterval h)
    initActiveIntervals = Set.empty

    lookupResType :: Node -> ResType h
    lookupResType n = case matchNodeInCG codeGraph n of
      (CGDataFlowNode (_,dfNode)) ->
        case dfNode of
          (ResourceNode resType) -> resType
          (InstructionNode instr) -> error
            $ "regAllocateLinearScanCG.lookupResType found instruction on node : "  ++ show n
          (BranchNode _ _ _) -> error
            $ "regAllocateLinearScanCG.lookupResType found branch on node : "  ++ show n
      (CGControlFlowNode _) -> error $ "regAllocateLinearScanCG.lookupResType found CFNode" ++ show n
      CGNotFound -> error $ "regAllocateLinearScanCG.lookupResType bad node lookup" ++ show n

    nodeIsRegType :: RegType h -> ResType h -> Bool
    nodeIsRegType regType resType = case resType of
                                      (RegisterRes regType') -> regType == regType'
                                      (MemoryRes _) -> regType == memRegType
                                      _ -> False

    fixedNodes :: [(Node,ByteString)]
    fixedNodes = map (\(n,r) -> case IM.lookup n coalesceMap of
                                     Just n' -> (n',r)
                                     Nothing -> (n,r))
                    [ (n,r) | (s0,r) <- inputRegs, (s1,n) <- inpNodes, s0 == s1 ]

    fixedNodesRegMapByType :: RegType h -> RegMap h
    fixedNodesRegMapByType regType =
      Map.fromList
        $ map (\(n,b) -> (n,(regType,b)))
        $ filter (nodeIsRegType regType . lookupResType . fst)
          fixedNodes

    -- TODO: Handle spills
    spillNodes = []

    addToRegPool ::  ByteString -> [ByteString] -> [ByteString]
    addToRegPool reg regPool = reg : regPool

    removeFromRegPool :: ((Int, Int), Node)
      -> Set (ActiveInterval h)
      -> [ByteString]
      -> (ByteString, Set (ActiveInterval h), [ByteString])
    removeFromRegPool (interval,n) activeIntervals (reg:regs) =
      let
        newActiveInterval =
          ActiveInterval interval reg n

        newActiveIntervals =
          Set.insert newActiveInterval activeIntervals
      in
        (reg, newActiveIntervals, regs)

    insertFixedNodeAsActive :: ((Int, Int), Node)
      -> RegMap h
      -> Set (ActiveInterval h)
      -> [ByteString]
      -> (ByteString, Set (ActiveInterval h), [ByteString])
    insertFixedNodeAsActive (interval,n) regMap activeIntervals regPool =
      let
        fixedNodeReg =
          snd $ regMap Map.! n
        newActiveInterval =
          ActiveInterval interval fixedNodeReg n
        newActiveIntervals =
          Set.insert newActiveInterval activeIntervals
      in
        (fixedNodeReg, newActiveIntervals, regPool)

    foldIntervalFunc
      :: (RegType h, RegMap h, Set (ActiveInterval h), [ByteString])
      -> ((Int, Int), Node)
      -> (RegType h, RegMap h, Set (ActiveInterval h), [ByteString])
    foldIntervalFunc
      (regType, regMap, activeIntervals, regPool) interval@((start,end), nd) =
      let
        (newActiveIntervals, newRegPool) =
          expireOldIntervals interval (Set.toList activeIntervals) regPool
        (regName, newActiveIntervals', newRegPool') =
          if nd `Map.member` regMap
            then insertFixedNodeAsActive interval regMap (Set.fromList newActiveIntervals) newRegPool
            else removeFromRegPool interval (Set.fromList newActiveIntervals) newRegPool
        -- NOTE: For fixed nodes, don't reinsert
        newRegMap =
          if nd `Map.member` regMap
            then regMap
            else Map.insert nd (regType, regName) regMap
      in
        if null regPool
        then error $ "regAllocateLinearScanCG failed on node " ++ show nd
        else
          ( regType
          , newRegMap
          , newActiveIntervals'
          , newRegPool')

    expireOldIntervals :: ((Int,Int), Node)
      -> [ActiveInterval h]
      -> [ByteString]
      -> ([ActiveInterval h], [ByteString])
    expireOldIntervals _ [] regs = ([], regs)
    expireOldIntervals
      liveInterval@((start,end),n)
      activeIntervals@(ActiveInterval (start',end') reg nd:restIntervals)
      regPool =
        if end' >= start
        then (activeIntervals, regPool)
        else
          expireOldIntervals liveInterval restIntervals (addToRegPool reg regPool)

    linearScanRegAllocation :: RegType h -> RegMap h
    linearScanRegAllocation regType =
      let
        filteredIntervalByReg =
          filter
            (nodeIsRegType regType . lookupResType . snd)
            (Map.toList intervalMap)
        initRegMap =
          fixedNodesRegMapByType regType
        fixedNodesRegColors =
          map snd $ Map.elems initRegMap
        initRegPool =
          regColors regType List.\\ fixedNodesRegColors
        (_, regMap, _, _) =
          foldl
            foldIntervalFunc
            (regType, initRegMap `debug` ("initRegMap" ++ show initRegMap), initActiveIntervals, regColors regType)
            filteredIntervalByReg
      in
        regMap

    combinedRegMaps :: RegMap h
    combinedRegMaps =
      foldl Map.union Map.empty $
        map linearScanRegAllocation (allRegTypes @h)

    missingRegMaps = IM.map (\n -> case Map.lookup n combinedRegMaps of
                                    Just n' -> n'
                                    Nothing -> error $ "failed to lookup node: " ++ show n
                                               ++ "\nregCoalesceMap: " ++ show coalesceMap
                                               ++ "\ncombinedRegMaps: " ++ show combinedRegMaps
                            )
                     $ IM.filter (\x -> not (x `elem` spillNodes)) coalesceMap

    allRegMaps = (Map.fromList $ IM.toList missingRegMaps) `Map.union` combinedRegMaps
  in
    Right (allRegMaps, Map.empty, schedGraph)
      `debug` ("Interval map: \n" ++ show intervalMap)
      `debug` ("Regmap: \n" ++ show allRegMaps)

liveIntervals :: forall h . LivenessSets -> ScheduledGraph h -> Map (Int,Int) Node
liveIntervals liveSets schedGraph =
  let
    startNode = case topsort schedGraph of
      (n:_) -> n
      [] -> error $ "buildScheduledGraph constructed empty graph"
    initLiveRanges =
      case IM.lookup startNode (livenessIn liveSets) of
        Nothing -> error $ "liveIntervals failed for start node " ++ show startNode
        Just foundInitLiveRange ->
          IM.fromList . flip zip (repeat 0) . IS.toList $ foundInitLiveRange
  in
    liveIntervalsIterate initLiveRanges Map.empty 0 liveSets schedGraph startNode
      `debug` ("liveSets" ++ show liveSets)

liveIntervalsIterate :: forall h . IntMap Int
                     -> Map (Int, Int) Node
                     -> Int
                     -> LivenessSets
                     -> ScheduledGraph h
                     -> Node
                     -> Map (Int, Int) Node
liveIntervalsIterate liveRanges intervalMap iterNum liveSets schedGraph nd =
  let
    livenessLookup liveSet = case IM.lookup nd liveSet of
      Nothing -> error $ "livenesslookup failed for node " ++ show nd
      Just foundLiveSet -> foundLiveSet

    liveRangesLookup liveNd = case IM.lookup liveNd liveRanges of
      Nothing -> error $ "liveRangesLookup failed for node " ++ show liveNd ++
                         ", in liveRanges " ++ show liveRanges
      Just foundLiveNd -> foundLiveNd

    liveInNd, liveOutNd :: IntSet
    (liveInNd, liveOutNd) =
      (livenessLookup $ livenessIn liveSets, livenessLookup $ livenessOut liveSets)

    -- Map each newly defined live variable to the current time
    newLiveRanges :: IntMap Int
    newLiveRanges =
      IM.union liveRanges $
        IM.fromList
        (zip (IS.toList (liveOutNd `IS.difference` liveInNd)) (repeat iterNum))

    expiredRanges :: [Int]
    expiredRanges =
      IS.toList $ liveInNd `IS.difference` liveOutNd

    -- For each expired variable, extract its defined start time from
    -- `liveRanges`, attach the current time as its end time, and
    -- insert the interval which is mapped to the its register node
    -- NOTE: We're not deleting expired variables from `liveRanges`
    -- on the premise that we save on computation since it does not affect
    -- the overall algorithm
    newIntervals, newIntervalMap :: Map (Int, Int) Node
    newIntervals =
      Map.fromList
        [ ((startTime, iterNum), regNode)
        | (regNode, startTime) <- zip expiredRanges (map liveRangesLookup expiredRanges) ]
        -- $ zip (map (liveRanges IM.!) expiredRanges) (repeat iterNum) -- (start,end)
        -- $ expiredRanges -- register of interest
    newIntervalMap =
      Map.union intervalMap newIntervals

    -- Retrieve the next node to process. The time is incremented
    -- in the recursive call
    -- TODO: Does branching work properly? i.e. more than one next node
    nextNodes = suc schedGraph nd
  in case nextNodes of
    [] -> newIntervalMap
    allNextNodes ->
      -- (flip debug) ("newLiveRanges: " ++ show newLiveRanges) $
      foldr1 Map.union $
        map
        (liveIntervalsIterate
           newLiveRanges newIntervalMap (iterNum + 1) liveSets schedGraph)
        allNextNodes

-- Have an interval map
-- defined as (registerNode, (instrDef, instrExpired))
-- i.e. maps register nodes to an interval of its lifetime
-- then the linear time scan algorithm uses this interval
-- to determine register allocation

regMapLookup n regMap = case Map.lookup n regMap of
                          Just r -> r
                          Nothing -> error $ "Bad lookup on node: " ++ show n
                                    ++ "\nAnd regMap: " ++ show regMap
