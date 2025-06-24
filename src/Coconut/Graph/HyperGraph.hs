-- |
-- Module      :  HyperGraph
-- Copyright   :  (c) OCA 2021
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module provides an fgl Graph (using Data.Graph.Inductive.PatriciaTree) that uses
-- @HyperEdge@'s' as edge labels providing an identifier to unify multiple labelled
-- edges (i.e., @LEdge@'s) into a single hyper edge

-- TODO remove me?
module Coconut.Graph.HyperGraph where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

-- * HyperGraph Data Types

-- | A HyperGraph is a @Gr@ with a @HyperEdge b@ for edges that provides a unique identifier
--   for each edge (i.e., via @edgeID@)
type HyperGraph n el = Gr n (HyperEdge el)

-- | A HyperEdge is just any edge label with a unique ID (i.e., @edgeID@)
data HyperEdge el = HyperEdge { hyperEdgeID :: Int
                              , hyperEdgeLabel :: el }
  deriving Show

-- | A HyperEdge is @Eq@ by its @edgeID@ alone
instance Eq (HyperEdge b) where
  (HyperEdge id0 _) == (HyperEdge id1 _) = id0 == id1

-- | A HyperEdge is @Ord@ by its @edgeID@ alone
instance Ord (HyperEdge b) where
  (HyperEdge id0 _) <= (HyperEdge id1 _) = id0 <= id1

-- * Utility Functions

-- | Create a Map from @hyperEdgeID@ to @hyperEdgeLabel@ with input and output nodes (respectively)
--   Use this instead of @labEdges@ which will provide a list of hyperedges as if they were individual
--   edges
hyperEdges :: HyperGraph n el -> IntMap (el,[Node],[Node])
hyperEdges gr =
  let
    edgeList = map (\(n0,n1,e) -> (hyperEdgeID e,(hyperEdgeLabel e,[n0],[n1]))) $ labEdges gr
    combineLists (eLbl0,ins0,outs0) (eLbl1,ins1,outs1) = (eLbl0,ins0++ins1,outs0++outs1)
  in IM.fromListWith combineLists edgeList

-- * Example HyperGraph

-- | This following graph connects nodes a,b to c via hyper edge e
--    a ---\
--          |e|----> c
--    b ---/
testHyperGraph :: HyperGraph String String
testHyperGraph = mkGraph [(0,"a"),(1,"b"),(2,"c")]
                      [(0,2,HyperEdge 3 "e")
                      ,(1,2,HyperEdge 3 "e")]

testMatchAny :: GDecomp Gr String (HyperEdge String)
testMatchAny = matchAny testHyperGraph
