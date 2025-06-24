-- |
-- Module      :  Coconut.Rewrite
-- Copyright   :  (c) OCA 2022
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a functionality for performing CodeGraph rewrites (i.e., after construction)

{-# LANGUAGE ScopedTypeVariables #-}
module Coconut.Rewrite where

import qualified Data.List as List
import Data.Maybe (fromJust)

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)

import Coconut.BaseTypes
import Coconut.HashedSchedule

-- | Apply all rewrites
cgApplyRewrites :: Hardware h => CodeGraph h -> CodeGraph h
cgApplyRewrites = reMaterializeConstants

-- | Rematerialize constant loads
reMaterializeConstants :: Hardware h => CodeGraph h -> CodeGraph h
reMaterializeConstants cg =
  let
    dfGraphs = cgDataFlows cg
    maxID = cgMaxID cg
    -- map reMaterializeConstantsDF over dfGraphs (keeping track of a unique id while doings so)
    reBuildDataFlows ((lbl,dfGraph0):dfGraphs0) dfGraphs1 uID0 =
      case reMaterializeConstantsDF (dfGraph0,uID0) of
        (dfGraph1,uID1) -> reBuildDataFlows dfGraphs0 ((lbl,dfGraph1):dfGraphs1) uID1
    reBuildDataFlows [] dfGraphs1 uID1 = (dfGraphs1,uID1)

    (dfGraphs',maxID') = reBuildDataFlows dfGraphs [] maxID
  in cg { cgDataFlows = dfGraphs', cgMaxID = maxID' }

reMaterializeConstantsDF :: forall h. Hardware h => (DataFlowGraph h,Int) -> (DataFlowGraph h,Int)
reMaterializeConstantsDF ((DataFlowGraph dfGraph dfInps dfOuts dfOverwrites dfIncr dfStage),maxID) =
  let
    allNodes = labNodes dfGraph
    allEdges = labEdges dfGraph
    -- all constant load instructions
    constantLoads :: [LNode (DFNode h)]
    constantLoads = filterByName (\name -> case name of
                                     'u':'n':_ -> True
                                     _ -> False) allNodes
    -- all pairs of constant loads nodes with the register node they produce
    loadsWithRegs :: [(Node,Node)]
    loadsWithRegs = concatMap (\(n0,n1,_) -> if n0 `elem` (map fst constantLoads)
                                               then [(n0,n1)]
                                               else []) allEdges

    -- iterate through loadsWithRegs and build a new dfGraph with constant nodes reassigned
    reAssignConstants :: ((Gr (DFNode h) DFEdge),Int)
                      -> [(Node,Node)]
                      -> ((Gr (DFNode h) DFEdge),Int)
    reAssignConstants (dfGraph0,uID) []                         = (dfGraph0,uID)
    reAssignConstants (dfGraph0,uID) ((loadNode,regNode):edges) =
      let
        constantConsumers0 = concatMap (\(rN,iN,idx) -> if rN == regNode then [(iN,idx)] else []) allEdges
        constantConsumers = case constantConsumers0 of
                              (_:cs) -> cs
                              [] -> []
        (dfGraph1,uID') = foldr (\(iN,idx) (dfGraphN,uID) ->
                                   reAssignConstant (dfGraphN,uID) (loadNode,regNode,iN,idx))
                                (dfGraph0,uID) constantConsumers
      in reAssignConstants (dfGraph1,uID') edges

    -- given a load -> reg -> consumer triple
    --   delete the edge (reg,consumer)
    --   insert new load',reg' nodes
    --   add edges load' -> reg' -> consumer (making sure to keep the correct dfEdgeIndex from reg' -> cons)
    reAssignConstant :: ((Gr (DFNode h) DFEdge),Int)
                     -> (Node,Node,Node,DFEdge)
                     -> ((Gr (DFNode h) DFEdge),Int)
    reAssignConstant (dfGraph0,uID) (loadNode,regNode,consumerNode,dfEdgeIdx) =
      let
        newLoad = (uID,fromJust $ List.lookup loadNode allNodes)
        newReg = (uID+1,fromJust $ List.lookup regNode allNodes)
        dfGraph1 = insEdge (uID+1,consumerNode,dfEdgeIdx) -- edge from newReg -> consumerNode
                 $ insEdge (uID,uID+1,DFEdge 0) -- edge from newLoad -> newReg
                 $ insNode newLoad
                 $ insNode newReg
                 $ delEdge (regNode,consumerNode) dfGraph0
      in (dfGraph1,uID+2)

    (dfGraph',maxID') = reAssignConstants (dfGraph,maxID+1) loadsWithRegs

    -- TODO what to do about inputs that are from constant loads?
  in (DataFlowGraph dfGraph' dfInps dfOuts dfOverwrites dfIncr dfStage,maxID')
