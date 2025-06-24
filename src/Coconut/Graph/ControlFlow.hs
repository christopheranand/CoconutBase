-- |
-- Module      :  ControlFlowGraph
-- Copyright   :  (c) OCA 2021
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module provides an fgl Graph (using Data.Graph.Inductive.PatriciaTree) that uses
-- FIXME ControlFlow module description

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Coconut.Graph.ControlFlow where

import Data.List (nub)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Coconut.BaseTypes
import Coconut.Graph.DataFlow

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree


-- | Return all @LNode@'s in a @ControlFlowGraph@
cfNodes :: ControlFlowGraph h -> [(Node,CFNode)]
cfNodes (ControlFlowGraph cfGraph _ _) = labNodes cfGraph

-- | Return all @LEdge@'s in a @ControlFlowGraph@
cfEdges :: ControlFlowGraph h -> [(Node,Node,CFEdge h)]
cfEdges (ControlFlowGraph cfGraph _ _) = labEdges cfGraph

-- | Lookup a @Node@ in a @ControlFlowGraph@ and return it's corresponding @CFNode@
matchCFNode :: Node -> ControlFlowGraph h -> Maybe CFNode
matchCFNode n cfGraph =
  let
    fglGraph = controlFlowGraph cfGraph
  in case match n fglGraph of
       (Just (_,_,cfNode,_),graph') -> Just cfNode
       (Nothing,graph') -> Nothing
