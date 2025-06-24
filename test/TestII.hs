{-|
Module      : test.TestII
Copyright   :  (c) Curtis D'Alves 2023
License     :  GPL (see the LICENSE file)
Maintainer  :  curtis.dalves@gmail.com
Stability   :  experimental
Portability :  portable

Description : Test module containing functions for testing the computation of
ideal II for modulo scheduling
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TestII where

import Coconut.BaseTypes
import Coconut.Utils.CGWrappers
import Coconut.Graph.DataFlow
import Coconut.Graph.ControlFlow
import Coconut.Graph.CodeGraph
import Coconut.Core.CoreISA
import Coconut.Core.ControlISA (compose,branch,doWhile)
import Coconut.Core.CoreHardware (CORE)
import Coconut.Core.MetaData
import Coconut.HashedSchedule
import Coconut.Graph.Dot

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

import Data.Graph.Inductive.Graph hiding (Graph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (sp)
import Data.Graph.Inductive.Query.DFS (topsort)
import Data.Graph.Inductive.Query.BFS 

testBlock :: forall repr . CoreISA repr => repr GPR -> repr GPR
testBlock g0 =
  let
    g1 = addG g0 g0
    c  = unintegerG 0
    g2 = addG c g1
    g3 = addG c g2
    g4 = addG c g3
    g5 = addG g1 g4
  in g5


testBasicBlock :: CodeGraph CORE
testBasicBlock = createCG (genBlock (basicBlock "block" ["g0"] ["g3"] (testBlock @(Graph CORE))))

latencyGraph :: forall h . Hardware h => CodeGraph h -> Gr () Integer
latencyGraph (CodeGraph cfGraph dataFlows cgIn cgOut mrTables constTable tags maxID dbgMap) =
  let
    metaData = coreMetaData

    ((dfLabel,dataFlow ):_) = dataFlows
    (DataFlowGraph dfGraph dfInps dfOuts dfOverwrites dfIncr dfStage) = dataFlow

    isInstruction :: Node -> Bool
    isInstruction node =
      case match node dfGraph of
        (Just (_,_,InstructionNode el,_),_) ->
          case el of
            (Instruction _ _ _) -> True
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
        allInstrNodes = filter (isInstruction) $ topsort dfGraph
      in [ lNode | lNode <- labNodes (dfGraph), (fst lNode) `elem` allInstrNodes ]


    dependencies = mkDependencies dataFlow allInstructions
    latEdges = map (\(i,j) -> (i,j,(latOrErr metaData dataFlow i))) dependencies
    latNodes = concatMap (\(i,j) -> [(i,()),(j,())]) dependencies

  in mkGraph latNodes latEdges


-- | Method 1
-- This method uses pathTree and is way to slow
longestPath0 :: (Num b, DynGraph g, Ord b) =>
               Node -> Node -> g a b -> (([Node], b), [[Node]])
longestPath0 s t graph =
  let
    pathsToT = map (\p -> (takeWhile (/=t) p) ++ [t])
             $ filter (\p -> t `elem` p)
             $ pathTree s graph

    lEdges = labEdges graph

    pathWeight (n0:n1:p) =
      case filter (\(n0',n1',_) -> (n0,n1) == (n0',n1')) lEdges of
        [(_,_,w)] -> w + pathWeight (n1:p)
        _ -> 0
    pathWeight _ = 0

  in case reverse $ List.sortOn snd $ map (\p -> (p, pathWeight p)) pathsToT of
       (pm:_) -> (pm,pathsToT)
       _ -> error "TODO"

-- | Find all possible paths from this given node, avoiding loops,
--   cycles, etc.
pathTree :: DynGraph g => Node -> g a b -> [[Node]]
pathTree n g = pathTree' $ match n g

pathTree'             :: (DynGraph g) => Decomp g a b -> [[Node]]
pathTree' (Nothing,_) = []
pathTree' (Just ct,g)
    | isEmpty g = []
    | null sucs = [[n]]
    | otherwise = (:) [n] . map (n:) . concatMap (subPathTree g') $ sucs
    where
      n = node' ct
      sucs = suc' ct
      -- Avoid infinite loops by not letting it continue any further
      ct' = makeLeaf ct
      g' = ct' & g
      subPathTree gr n' = pathTree' $ match n' gr

-- | Remove all outgoing edges
makeLeaf           :: Context a b -> Context a b
makeLeaf (p,n,a,_) = (p', n, a, [])
    where
      -- Ensure there isn't an edge (n,n)
      p' = filter (\(_,n') -> n' /= n) p

-- | Method 2
--
-- This method uses a linear algorithm based on first doing a topological sort
-- NOTE you can use this map to find the maximum path by traversing the graph in
-- reverse, picking the predessor with the maximum cost at each point.
longestPath :: (DynGraph gr, Num a1, Ord a1) => gr a2 a1 -> Map Node a1
longestPath graph =
  let
    nodes = topsort graph
  in computeMaxMap nodes graph Map.empty

