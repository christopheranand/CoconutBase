-- |
-- Module      :  DataFlow
-- Copyright   :  (c) OCA 2021
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module provides a BiPartite Graph that models data flow (i.e. basic blocks)
-- as sets of nodes that are resources (i.e, registers, memory regions, etc) connected
-- to instructions. The underlying graph implementation uses fgl (i.e., Martin Erwigs
-- functional graph library)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Coconut.Graph.DataFlow where

import Coconut.BaseTypes

import Data.Maybe (fromJust)
import qualified Data.List as List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map.Strict (Map)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Map.Strict as Map
import Data.List.Split

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Coconut.BaseTypes (ResType,EL (..), MDMap (..),Hardware (..))
import Data.Maybe (isNothing)


-- | Lookup a @Node@ in a @DataFlowGraph@ and return it's corresponding @DFNode@
matchDFNode :: Node -> DataFlowGraph h -> Maybe (DFNode h)
matchDFNode n dfGraph =
  let
    fglGraph = dataFlowGraph dfGraph
  in case match n fglGraph of
       (Just (_,_,dfNode,_),graph') -> Just dfNode
       (Nothing,graph') -> Nothing

-- | Lookup a @Node@ in a @DataFlowGraph@ for its @ResType@ (if the node is not
-- a @ResourceNode@ it will return @Nothing@)
matchResType :: Node -> DataFlowGraph h -> Maybe (ResType h)
matchResType n dfGraph = case matchDFNode n dfGraph of
  Just (ResourceNode res) -> Just res
  _ -> Nothing

-- | Identifies if a DFNode is a @Resource@
isResource :: DFNode h -> Bool
isResource dfNode = case dfNode of
                      ResourceNode _ -> True
                      _ -> False

-- | Identifies if a DFNode is an @Instruction@
isInstruction :: DFNode h -> Bool
isInstruction dfNode = case dfNode of
                         InstructionNode _ -> True
                         _ -> False

-- | Identifies if a DFNode is an @InitMR@
isInitMR :: DFNode h -> Bool
isInitMR dfNode = case dfNode of
                    InstructionNode (InitMR _ _) -> True
                    _ -> False

-- | Return a list of all @InitMR@ labeled nodes
initMRNodes :: DataFlowGraph h -> [LNode (DFNode h)]
initMRNodes dfGraph =
  let
    allNodes = labNodes $ dataFlowGraph dfGraph
    initMRNodes = filter (\(_,dfNode) -> isInitMR dfNode) allNodes
  in initMRNodes

-- | Return a list of all @Resource@ nodes in a given @DataFlowGraph@
resourceNodes :: DataFlowGraph h -> [(Node, ResType h )]
resourceNodes dfGraph =
  let
    graph = dataFlowGraph dfGraph
    unWrapResource (node,dfNode) = case dfNode of
                              (ResourceNode rs) -> [(node,rs)]
                              _ -> []
  in concatMap unWrapResource $ labNodes graph

-- | Return a list of all @Instruction@ nodes in a given @DataFlowGraph@
instructionNodes :: DataFlowGraph h -> [(Node,EL h)]
instructionNodes dfGraph =
  let
    graph = dataFlowGraph dfGraph
    unWrapInstruction (node, dfNode ) = case dfNode of
                              (InstructionNode instr) -> [(node,instr)]
                              _ -> []
  in concatMap unWrapInstruction $ labNodes graph

-- | Return all list of all @BranchNode@'s in a given @DataFlowGraph@
branchNodes :: DataFlowGraph h -> [Node]
branchNodes dfGraph =
  let
    graph = dataFlowGraph dfGraph
    unWrapInstruction (node, dfNode ) = case dfNode of
                              (BranchNode _ _ _) -> [node]
                              _ -> []
  in concatMap unWrapInstruction $ labNodes graph

-- | Filter a @DataFlowGraph@ for @DataFlowContexts@ that satisfy a given predicate
filterDFContexts :: (DataFlowContext h -> Bool) -> DataFlowGraph h -> [DataFlowContext h]
filterDFContexts p (DataFlowGraph dfGraph inps outs overwrites incr stage)
  | isEmpty dfGraph =
      case matchAny dfGraph of
        (ctx,dfGraph') ->
          if p ctx
           then ctx : resourceContexts (DataFlowGraph dfGraph' inps outs overwrites incr stage)
           else resourceContexts (DataFlowGraph dfGraph' inps outs overwrites incr stage)
  | otherwise = []


-- | Return a list of all @Resource@ contexts (i.e. from fgl @Context@)
--   in a given @DataFlowGraph@
resourceContexts :: DataFlowGraph h -> [DataFlowContext h]
resourceContexts dfGraph = filterDFContexts (\(_,_,dfNode,_) -> isResource dfNode) dfGraph

-- | Return a list of all @Instruction@ contexts (i.e. from fgl @Context@)
--   in a given @DataFlowGraph@
instructionContexts :: DataFlowGraph h -> [DataFlowContext h]
instructionContexts dfGraph = filterDFContexts (\(_,_,dfNode,_) -> isInstruction dfNode) dfGraph

-- | Returns true if the given @DataFlowGraph@ contains the given @Node@
isElemOfDF :: DataFlowGraph h -> Node -> Bool
isElemOfDF dfGraph node = case match node (dataFlowGraph dfGraph) of
                            (Just _,_) -> True
                            (Nothing,_) -> False

-- | Returns all the instruction @Node@'s that consume any registers produced by
-- the given instruction @Node@ by finding the successors of the successors of
-- the given @Node@
instructionConsumers :: DataFlowGraph h -> Node -> [Node]
instructionConsumers dfGraph node =
  let
    successors = suc (dataFlowGraph dfGraph)
  in concatMap successors $ successors node

-- | Returns all the instruction @LNode@'s that consume any registers produced by
-- the given instruction @Node@ by finding the successors of the successors of
-- the given @Node@
instructionConsumersL :: DataFlowGraph h -> Node -> [LNode (DFNode h)]
instructionConsumersL dfGraph node =
  let
    allNodes = labNodes $ dataFlowGraph dfGraph
    successors = suc (dataFlowGraph dfGraph)
    consumers = concatMap successors $ successors node
  in map (\n -> (n,fromJust $ List.lookup n allNodes)) consumers

-- | Returns all the instruction @Node@'s that produce any registers consumed by
-- the given instruction @Node@ by finding the predecessors of the predecessors of
-- the given @Node@
instructionProducers :: DataFlowGraph h -> Node -> [Node]
instructionProducers dfGraph node =
  let
    producers = pre (dataFlowGraph dfGraph)
  in concatMap producers $ producers node

-- | Returns all the instruction @LNode@'s that consume any registers produced by
-- the given instruction @Node@ by finding the successors of the successors of
-- the given @Node@
instructionProducersL :: DataFlowGraph h -> Node -> [LNode (DFNode h)]
instructionProducersL dfGraph node =
  let
    allNodes = labNodes $ dataFlowGraph dfGraph
    previous = pre (dataFlowGraph dfGraph)
    producers = concatMap previous $ previous node
  in map (\n -> (n,fromJust $ List.lookup n allNodes)) producers

-- | Lookup an instructions latency in a given @MDMap@ given a @DataFlowGraph@
-- and the instructions @Node@
instructionLatency :: Hardware h => DataFlowGraph h -> MDMap h -> Node -> Int
instructionLatency dfGraph metaData node =
  case match node (dataFlowGraph dfGraph) of
        (Just (_,_,InstructionNode el,_),_) ->
          case el of
            (Instruction _ name _) ->
              case Map.lookup name $ mdMap metaData of
                Just mD -> hardwareLatency mD
                Nothing -> error $ "instructionLatency couldn't find: " ++ name
            (Spill name)  ->
              case Map.lookup name $ mdMap metaData of
                Just mD -> hardwareLatency mD
                Nothing -> error $ "instructionLatency couldn't find: " ++ name
            (Despill name)  ->
              case Map.lookup name $ mdMap metaData of
                Just mD -> hardwareLatency mD
                Nothing -> error $ "instructionLatency couldn't find: " ++ name
            _ -> 0
        (Just (_,_,BranchNode name _ _,_),_) ->
          case Map.lookup name $ mdMap metaData of
                Just mD -> hardwareLatency mD
                Nothing -> error $ "instructionLatency couldn't find: " ++ name
        _ -> error $ "bad node given to instructionLatency: " ++ show node


-- | Returns a @Map@ of latency depths for a given list of instructions (i.e., a
-- @[Node]@ all assummed to be instructions). NOTE the calculations are closed
-- under the given list of instructions
latencyDepth :: Hardware h
             => DataFlowGraph h
             -> [Node]
             -> (Node -> Int)
             -> Map.Map Node Int
latencyDepth dfGraph instrs latency = foldl depth Map.empty instrs
  where
    maximumWithDefault a [] = a
    maximumWithDefault _ lst = maximum lst
    depth hs node = let srcs = filter (`elem` instrs) $ instructionProducers dfGraph node
                    in if srcs == []
                      then Map.insert node (latency node) hs
                      else if all (flip Map.member hs) srcs
                            then Map.insert node
                                  (latency node + maximumWithDefault 0 (map (hs Map.!) srcs)) hs
                            else depth (foldl depth hs srcs) node


-- | Returns a @Map@ of latency heights for a given list of instructions (i.e.,
-- a @[Node]@ all assummed to be instructions). NOTE the calculations are closed
-- under the given list of instructions
latencyHeight :: Hardware h
              => DataFlowGraph h
              -> [Node]
              -> (Node -> Int)
              -> Map.Map Node Int
latencyHeight dfGraph instrs latency = foldl height Map.empty instrs
  where
    maximumWithDefault a [] = a
    maximumWithDefault _ lst = maximum lst
    height hs node =
      let targets = filter (`elem` instrs) $ instructionConsumers dfGraph node
       in if null targets
            then Map.insert node (latency node) hs
            else
              if all (`Map.member` hs) targets
                then
                  Map.insert
                    node
                    (latency node + maximumWithDefault 0 (map (hs Map.!) targets))
                    hs
                else height (foldl height hs targets) node

-- | Partitions a @DataFlowGraph@ by instruction nodes (i.e., @InstructionNode@
-- and @BranchNode@) that satisfy a given predicate. @ResourceNode@'s are
-- included if they are attached to an included instruction node, and the output
-- resource nodes of the first subgraph will be the same as the input resource
-- nodes of the second subgraph
partGraphByInstr :: forall h . Hardware h =>
  (String,DataFlowGraph h) -> (Node -> Bool) -> (DataFlowGraph h,DataFlowGraph h)
partGraphByInstr (label,dfGraph@(DataFlowGraph fglGraph dfIns dfOuts overwrites incr stage)) predicate =
  let
    subGraphFilter :: (Node -> Bool) -> LNode (DFNode h) -> Bool
    subGraphFilter p (n,dfNode) = case dfNode of
      (ResourceNode _) -> case match n fglGraph of
        -- If a ResourceNode is "floating" (i.e. has no instructions) include it
        (Just ([],_,_,[]),_) -> True
         -- if a ResourceNode is used or defined by any instruction satisfied by p, include it
        (Just (prevs,_,_,succs),_) -> (or $ map p $ (map snd prevs) ++ (map snd succs))
                                   || n `elem` (map snd dfOuts)
        (Nothing,_) -> error $ "partGraphByInstr.subGraphFilter bad node lookup: " ++ show n
      -- InstructionNode/BranchNode are included if satisfied by p
      _ -> p n

    isSpanningConstLoad p (n,dfNode) = case dfNode of
      (ResourceNode _) -> case match n fglGraph of
         -- include resource node if it was produced by a constant load and
         -- it has a consumer not satisfied by p
        (Just (prevs,_,_,succs),_) -> (or $ map (\(_,n) -> isConstantLoad dfGraph n) prevs)
                                   && (or $ map (\(_,n) -> not $ p n) succs)
        (Nothing,_) -> error $ "partGraphByInstr.subGraphFilter bad node lookup: " ++ show n
      -- include an Instruction/Branch node if it is a constant load that has its produced
      -- register consumed by an instruction not satisfied by p
      _ -> isConstantLoad dfGraph n && (or $ map (not . p) $ instructionConsumers dfGraph n)

    -- filter out nodes that satisify subGraphFilter predicate for 0, and dont satisfy for 1
    lNodes,lNodes0,lNodes1 :: [LNode (DFNode h)]
    lNodes = labNodes fglGraph
    lNodes0 = filter (\lN -> subGraphFilter predicate lN
                          && not (isSpanningConstLoad predicate lN)) lNodes
    lNodes1 = filter (\lN -> subGraphFilter (not . predicate) lN
                          || isSpanningConstLoad predicate lN) lNodes

    -- partition graph by filtered nodes
    fglGraph0,fglGraph1 :: Gr (DFNode h) DFEdge
    fglGraph0 = subgraph (map fst lNodes0) fglGraph
    fglGraph1 = subgraph (map fst lNodes1) fglGraph

    regProducedByConstLoad n = (or $ map (isConstantLoad dfGraph) $ pre fglGraph n )

    -- all nodes produced by an initMR node
    initRegs = concatMap (\(n,_) -> suc fglGraph n) $ initMRNodes dfGraph
    -- add all overwrites of those nodes
    allInitRegs = IntSet.toList $ mconcat $ map (\n -> findAllOverwrites n overwrites) initRegs

    -- find all nodes with no predecessor's in the sub-graphs, these are the new inputs
    -- NOTE also need to filter out constant loads
    dfIns0 = dfIns

      -- map (addTag 0) $ map fst
      --                       $ filter (\(n,dfNode) ->
      --                                    isResource dfNode
      --                                   && (null $ pre fglGraph0 n)
      --                                   && not (regProducedByConstLoad n))
      --                       $ labNodes fglGraph0
    dfIns1 = map (addTag 1) $ map fst
                            $ filter (\(n,dfNode) ->
                                         isResource dfNode
                                        && (null $ pre fglGraph1 n)
                                        && not (regProducedByConstLoad n ))
                            $ labNodes fglGraph1
    -- find all nodes with no successor's in the sub-graphs, these are the new outputs
    dfOuts1 = map (addTag 1) $ filter (\n -> (null $ suc fglGraph1 n)
                                             && (not $ n `elem` allInitRegs))
                             $ map fst $ labNodes fglGraph1
    -- NOTE the standard definition of dfOuts0 commented out below will miss nodes that are consumed
    -- but are still needed by dfIns1
    -- dfOuts0 = map (addTag 0) $ filter (\n -> null $ suc fglGraph0 n) $ map fst $ labNodes fglGraph0
    dfOuts0 = dfIns1
    -- dfOuts0 = map (addTag 0) $ map fst
    --                          $ filter (\(n,dfNode) -> isResource dfNode
    --                                                   && n `elem` (map fst $ labNodes fglGraph1))
    --                          $ labNodes fglGraph0

    -- need to make sure nodes that are inputs and outputs need to get passed
    -- along even if they are not dangling in (i.e. have no successors) in fglGraph0
    inAndOut = [ inp | inp <- dfIns, inp `elem` dfOuts ]
    dfOuts0' = List.nub $ dfOuts0 ++ [ (tag,n) | (tag,n) <- inAndOut
                                               , not $ tag `elem` (map fst dfOuts0) ]
    dfIns1' = List.nub $ dfIns1 ++ [ (tag,n) | (tag,n) <- inAndOut
                                               , not $ tag `elem` (map fst dfIns1) ]
    dfOuts1' = List.nub $ dfOuts1 ++ [ (tag,n) | (tag,n) <- inAndOut
                                               , not $ tag `elem` (map fst dfOuts1) ]

    -- need to give new inputs/outputs tags, assign pre-existing tags when they exist
    addTag :: Int -> Int -> (String,Int)
    addTag stage n = case (tagLookup n dfIns,tagLookup n dfOuts) of
                       (Just tag,_) -> (tag,n)
                       (_,Just tag) -> (tag,n)
                       (Nothing,Nothing) -> (label++show n -- ++":"++show stage
                                            ,n)

    overwrites0 = filter (\(n,_) -> n `elem` map fst lNodes0) overwrites
    overwrites1 = filter (\(n,_) -> n `elem` map fst lNodes1) overwrites

    sharedRegs :: [LNode (DFNode h)]
    sharedRegs = filter (\(n,_) -> n `elem` map fst lNodes1) lNodes0

    dfGraph0 = DataFlowGraph fglGraph0 dfIns0 dfOuts0' overwrites0 incr stage
    dfGraph1 = DataFlowGraph fglGraph1 dfIns1' dfOuts1' overwrites1 incr stage

    tagLookup n ((lbl,n'):ns) = if n == n' then Just lbl else tagLookup n ns
    tagLookup n [] = Nothing

  in (dfGraph0,dfGraph1)
      -- `debug` ("\nRan partGraphByInstr with "
      --         ++"\ndfGraph0 dfIns " ++ show (dataFlowInputs dfGraph0)
      --         ++"\ndfGraph0 dfOuts " ++ show (dataFlowOutputs dfGraph0)
      --         ++"\ndfGraph1 dfIns " ++ show (dataFlowInputs dfGraph1)
      --         ++"\ndfGraph1 dfOuts " ++ show (dataFlowOutputs dfGraph1)
      --         ++"\n"
      --         )

-- | Iteratively partition a graph using partGraphByInstr and a predicate thats
-- applied from 0..n-1
partNGraphByInstr :: forall h . Hardware h
  => (String,DataFlowGraph h)
  -> (Int -> Node -> Bool)
  -> Int
  -> [DataFlowGraph h]
partNGraphByInstr (label,dfGraph) predicate n =
  let
    DataFlowGraph fglGraph dfIns dfOuts overwrites incr stage = dfGraph

    dfInTags = map fst dfIns
    dfOutTags = map fst dfOuts

    addStage :: Int -> (String,Node) -> (String,Node)
    addStage i (tag,n)
      | tag `elem` (dfInTags++dfOutTags) = (tag,n)
      | otherwise = (tag++":"++(show i),n)

    iteratePartition dfGraph0 i =
      let
        (dfGraphN0,dfGraphN1) = partGraphByInstr @h (label,dfGraph0) (predicate i)
      in if i < (n-1)
         then dfGraphN0 { dataFlowStage = i
                        , dataFlowInputs = map (addStage i) $ dataFlowInputs dfGraphN0
                        , dataFlowOutputs = map (addStage i) $ dataFlowOutputs dfGraphN0
                        } : iteratePartition dfGraphN1 (i+1)
         else [dfGraph0 { dataFlowStage = i
                        , dataFlowInputs = map (addStage i) $ dataFlowInputs dfGraph0
                        , dataFlowOutputs = map (addStage i) $ dataFlowOutputs dfGraph0
                        }]
  in iteratePartition dfGraph 0

-- | Recombine two DataFlowGraph's into a single DataFlowGraph that have been
-- partitioned by @partGraphByInstr@ or @partNGraphByInstr@
unPartDFGraph :: forall h . Hardware h
  => DataFlowGraph h
  -> DataFlowGraph h
  -> DataFlowGraph h
unPartDFGraph dfGraph0 dfGraph1 =
  let
    DataFlowGraph fglGraph0 dfIns0 dfOuts0 overwrites0 incr0 stage0 = dfGraph0
    DataFlowGraph fglGraph1 dfIns1 dfOuts1 overwrites1 incr1 stage1 = dfGraph1
    (nodes0,edges0) = (labNodes fglGraph0,labEdges fglGraph0)
    (nodes1,edges1) = (labNodes fglGraph1,labEdges fglGraph1)

    incr' = if incr0 == incr1
            then incr0
            else error "composeDFGraphs given two dfGraph's with different increments"
    overwrites' = overwrites0 `List.union` overwrites1
    -- enforce that the output nodes of dfGraph0 match the input nodes of dfGraph1
    tags0 = dataFlowOutputs dfGraph0
    tags1 = dataFlowInputs dfGraph1
    -- FIXME should we be tying back together with or without : now that it represents incr?
    -- they should all of the same incr anyways now?
    h tag = takeWhile (/=':') tag
    missingNodes0 = filter (\(lbl1,_) -> not $ (h lbl1) `elem` map (h . fst) tags0) tags1
    missingNodes1 = filter (\(lbl0,_) -> not $ (h lbl0) `elem` map (h . fst) tags1) tags0
    inOutTagsMatch = and [ n0 == n1 | (n0,n1) <- tieNodesByTag tags0 tags1 ]
                     && null missingNodes0 && null missingNodes1

    nodes' = if inOutTagsMatch
                then List.unionBy (\(n0,_) (n1,_) -> n0 == n1) nodes0 nodes1
                else error $ "unPartDFGraphs in out tags don't match:\ntags0 " ++ show tags0
                           ++ "\ntags1 " ++ show tags1
    edges' = edges0 `List.union` edges1

    fglGraph' = mkGraph nodes' edges'
  in DataFlowGraph fglGraph' dfIns0 dfOuts1 overwrites' incr' stage0

-- | Perform a fold recombining a list of @DataFlowGraph@s produced by
-- @partNGraphByInstr@
unPartDFGraphs :: forall h . Hardware h
  => [DataFlowGraph h]
  -> DataFlowGraph h
unPartDFGraphs [] = error "unPartDFGraphs given empty list"
unPartDFGraphs (dfGraph:dfGraphs) = foldl unPartDFGraph dfGraph dfGraphs

-- | Merge multiple DataFlowGraphs together to so their code all runs in
-- parallel. This will remap nodes in the second @DataFlowGraph@ to node+maxNode
-- and update the schedule to map to all nodes in the new @DataFlowGraph@ with
-- @dataFlowIncrement@ == 0
parallelMergeDFGraph :: forall h . Hardware h
  => (DataFlowGraph h,DataFlowGraph h)
  -> Schedule
  -> [String]
  -> (DataFlowGraph h,Schedule)
parallelMergeDFGraph (dfGraph0,dfGraph1) sched unionTags =
  let
    DataFlowGraph fglGraph0 dfIns0 dfOuts0 overwrites0 incr0 stage0 = dfGraph0
    DataFlowGraph fglGraph1 dfIns1 dfOuts1 overwrites1 incr1 stage1 = dfGraph1
    (nodes0,edges0) = (labNodes fglGraph0,labEdges fglGraph0)
    (nodes1,edges1) = (labNodes fglGraph1,labEdges fglGraph1)
    maxNode = max (snd $ nodeRange fglGraph0) (snd $ nodeRange fglGraph1)

    -- remap fglGraph1 nodes
    -- unionNodes = List.nub $ concat
    --              [ IntSet.toList (findAllOverwrites n (overwrites0 ++ overwrites1))
    --              | (lbl,n) <- dfIns1++dfOuts1, lbl `elem` unionTags ]
    unionNodes = List.nub [ n | (lbl,n) <- dfIns1++dfOuts1, lbl `elem` unionTags]

    reMapNode n = if n `elem` unionNodes then n else n+maxNode
    unReMapNode n = if n `elem` unionNodes then n else n-maxNode

    nodes1' = map (\(n,dfNode) -> (reMapNode n,dfNode)) nodes1
    edges1' = map (\(n0,n1,eL) -> (reMapNode n0,reMapNode n1,eL)) edges1
    dfIns1' = map (\(s,n) -> (s,reMapNode n)) dfIns1
    dfOuts1' = map (\(s,n) -> (s,reMapNode n)) dfOuts1
    overwrites1' = map (\(n,(n0,n1)) -> (reMapNode n,(reMapNode n0,reMapNode n1))) overwrites1

    -- combine nodes/edges
    nodes' = List.unionBy (\(n0,_) (n1,_) -> n0 == n1) nodes0 nodes1'
    edges' = edges0 `List.union` edges1'

    -- create new DatFlowGraph
    -- TODO do i need to differentiate tags in different stages here? or is it correct not to?
    -- addStage stage tag = takeWhile (/=':') tag ++ ":" ++ show stage ++ dropWhile (/=':') tag
    -- reMapTag stage (tag,n) = if tag `elem` unionTags
    --                             then (tag,n)
    --                             else (addStage stage tag,n)
    fglGraph' = mkGraph nodes' edges'
    -- dfIns' = map (reMapTag stage0) dfIns0  `List.union` map (reMapTag stage1) dfIns1'
    -- dfOuts' = map (reMapTag stage0) dfOuts0 `List.union` map (reMapTag stage1) dfOuts1'
    dfIns' = dfIns0  `List.union` dfIns1'
    dfOuts' = dfOuts0 `List.union` dfOuts1'
    overwrites' = overwrites0 `List.union` overwrites1'
    incr' = 0
    dfGraph' = DataFlowGraph fglGraph' dfIns' dfOuts' overwrites' incr' stage0

    -- update schedule (will now support mapping from node-incr AND all nodes'-incr')
    instrs0 = filter (\(_,dfNode) -> isInstruction dfNode && not (isInitMR dfNode)) nodes0
    sched0 = Map.fromList $ map (\(n0,_) ->
                                    case Map.lookup (n0-incr0) sched of
                                      Just t -> (n0,t)
                                      Nothing -> error $ "sched0 missing node: " ++ show (n0-incr0)
                                      ) instrs0
    instrs1 = filter (\(_,dfNode) -> isInstruction dfNode && not (isInitMR dfNode)) nodes1'
    sched1 = Map.fromList $ map (\(n1,_) ->
                                     case Map.lookup ((unReMapNode n1)-incr1) sched of
                                       Just t -> (n1,t)
                                       Nothing -> error $ "sched1 missing node: "
                                                  ++ show ((unReMapNode n1)-incr1)
                                 ) instrs1
    sched' = sched `Map.union` sched0 `Map.union` sched1
   in (dfGraph',sched')
      -- `debug` ("\nRunning parallelMergeDFGraphs with "
      --         ++"\ndfGraph0 dfIns " ++ show dfIns0
      --         ++"\ndfGraph0 dfOuts " ++ show dfOuts0
      --         ++"\ndfGraph1 dfIns " ++ show dfIns1
      --         ++"\ndfGraph1 dfOuts " ++ show dfOuts1
      --         ++"\n"
      --         )

-- | Fold over a list of @DataFlowGraph@s performing a @parallelMergeDFGraph@
-- (and build a new schedule)
parallelMergeDFGraphs :: Hardware h
                      => [DataFlowGraph h]
                      -> Schedule
                      -> [String]
                      -> (DataFlowGraph h,Schedule)
parallelMergeDFGraphs [] _ _ = error "parallelMergeDFGraphs given empty list"
parallelMergeDFGraphs (dfGraph:dfGraphs) sched unionTags =
  let
    parallelMergeDFGraphs' g0 [] s = (g0,s)
    parallelMergeDFGraphs' g0 (g:gs) s =
      let
        (g',s') = parallelMergeDFGraph (g0,g) s unionTags
      in parallelMergeDFGraphs' g' gs s'
  in parallelMergeDFGraphs' dfGraph dfGraphs sched

-- | Given a node and a list of overwrites return an IntSet of that node and all
-- its overwrites
findAllOverwrites :: Node -> [(Node,(Node,Node))] -> IntSet
findAllOverwrites node overwrites = groupOverwrites $ IntSet.fromList [node]
  where
    groupOverwrites set =
      let
        set' = foldr (IntSet.insert) set
               $ concatMap (\(n0,n1) ->
                               if n0 `IntSet.member` set || n1 `IntSet.member` set
                               then [n0,n1]
                               else []) $ map snd overwrites
      in if set == set' then set else groupOverwrites set'

-- | Given a data flow graph, return a set of all nodes with an tags that being
-- with "mrIn" or "mrOut" AND all of their overwrites (including overwrites of
-- those overwrites and so on)
findAllInOutMRNodes :: DataFlowGraph h -> IntSet
findAllInOutMRNodes dfGraph =
  let
    inMRNode = case filter (\(label,n) ->
                              case label of
                                ('m':'r':'I':'n':_) -> True
                                _ -> False)  (dataFlowInputs dfGraph) of
                 (n:_) -> snd n
                 [] -> error "findAllInOutMRNodes dataflowgraph contains no mrIn input"
    outMRNode = case filter (\(label,n) ->
                              case label of
                                ('m':'r':'O':'u':'t':_) -> True
                                _ -> False)  (dataFlowInputs dfGraph) of
                 (n:_) -> snd n
                 [] -> error "findAllInOutMRNodes dataflowgraph contains no mrOut input"
    allInMRNodes = findAllOverwrites inMRNode $ dataFlowOverwrites dfGraph
    allOutMRNodes = findAllOverwrites outMRNode $ dataFlowOverwrites dfGraph
  in allInMRNodes `IntSet.union` allOutMRNodes

-- | Zips nodes together that have the same tags. NOTE throws away Nodes that
-- don't have overlapping tags and doesn't maintain any specific order
tieNodesByTag :: [(String,Node)] -> [(String,Node)] -> [(Node,Node)]
tieNodesByTag taggedNodes0 taggedNodes1 =
  [(n0,n1) | (s0,n0) <- taggedNodes0
           , (s1,n1) <- taggedNodes1
           , takeWhile (/=':') s0 == takeWhile (/=':') s1]

-- | Same as @tieNodesByTag@ but throws an error if a label from taggedNodes1
-- is missing from taggedNodes0
tieNodesByTagSafe :: (String,[(String,Node)]) -> (String,[(String,Node)]) -> [(Node,Node)]
tieNodesByTagSafe (lbl0,taggedNodes0) (lbl1,taggedNodes1) =
   let
     h tag = takeWhile (/=':') tag
     missingNodes = filter (\(lbl1,_) -> not $ (h lbl1) `elem` map (h . fst) taggedNodes0) taggedNodes1
   in if null missingNodes
         then [(n0,n1) | (s0,n0) <- taggedNodes0
                       , (s1,n1) <- taggedNodes1
                       , h s0 == h s1]
         else error $ "tieNodesByTagSafe has missing Nodes " ++ show missingNodes
                    ++ "\n" ++ show lbl0 ++ ": " ++ show taggedNodes0
                    ++ "\n" ++ show lbl1 ++ ": " ++ show taggedNodes1

-- | Tie nodes by comparing tags with a given comparison operand
tieNodesBy :: (String -> String -> Bool)
           -> (String,[(String,Node)])
           -> (String,[(String,Node)])
           -> [(Node,Node)]
tieNodesBy cmp (lbl0,taggedNodes0) (lbl1,taggedNodes1) =
  let
    cmpTags (tag1,node1) = [ (node0,node1) | (tag0,node0) <- taggedNodes0
                                           , cmp tag0 tag1 ]
        -- [] -> error $ "tieNodesBy "
        --       ++ show (lbl0,lbl1) ++ " given unmatched tag " ++ show tag1
        -- xs -> xs
    tagPairs = concatMap cmpTags taggedNodes1
    -- missingNodes = [ (node0,node0) | (tag0,node0) <- taggedNodes0
    --                                , not $ node0 `elem` (map fst tagPairs) ]
  in tagPairs

-- | Tie nodes by matching tags
tieNodesById :: (String,[(String,Node)])
             -> (String,[(String,Node)])
             -> [(Node,Node)]
tieNodesById = tieNodesBy (==)

-- | Tie nodes by matching tags, for tags with lbl:i in tags0 match to lbl:i+1
tieNodesByModulo :: (String,[(String,Node)])
                 -> (String,[(String,Node)])
                 -> [(Node,Node)]
tieNodesByModulo tags0 tags1 = tieNodesBy cmpTagsByStage tags0 tags1

-- | Match two tags with lbl:i in tags0 match to lbl:i+1
cmpTagsByStage :: String -> String -> Bool
cmpTagsByStage =
  let
    cmp tag0 tag1 = if ':' `elem` tag0
                    then case splitIncr tag0 of
                              (tag0',i) -> tag0'++":"++show (i+1) == tag1
                    else tag0 == tag1

    splitIncr tag = case splitOn ":" tag of
                      (t:incr:_) -> (t,abs $ read incr :: Int)
                      _ -> error $ "splitIncr called on tag with no incr: " ++ show tag
  in cmp

tieNodesByTagModulo :: (String,[(String,Node)]) -> (String,[(String,Node)]) -> [(Node,Node)]
tieNodesByTagModulo (lbl0,taggedNodes0) (lbl1,taggedNodes1) =
  let
    noIncrNodes0 = [ (t0,n0) | (t0,n0) <- taggedNodes0, not (':' `elem` t0) ]
    noIncrNodes1 = [ (t1,n1) | (t1,n1) <- taggedNodes1, not (':' `elem` t1) ]
    noIncrZipped = [ (n0,n1) | (t0,n0) <- noIncrNodes0, (t1,n1) <- noIncrNodes1, t0 == t1 ]

    splitIncr tag = case splitOn ":" tag of
                      (t:incr:_) -> (t,abs $ read incr :: Int)
                      _ -> error $ "splitIncr called on tag with no incr: " ++ show tag

    -- if nodes have an incr, say
    -- tags0 = [("cosI:1",500),("cosI:0",501),("cosN:0",502),("cosN:-1",503)]
    -- tags1 = [("cosI:2",504),("cosI:3",505),("cosN:3",506),("cosN:4",507)]
    -- zip nodes together in sorted abs value of incr
    -- [(500,505),(501,505),(502,506),(503,507)]
    incrNodes0 = map (\(t0,n0) -> (splitIncr t0,n0)) [ (t0,n0) | (t0,n0) <- taggedNodes0, ':' `elem` t0 ]
    incrNodes1 = map (\(t1,n1) -> (splitIncr t1,n1)) [ (t1,n1) | (t1,n1) <- taggedNodes1, ':' `elem` t1 ]

    pullTags xs = case xs of
                    ((tag,incr),n):_ -> (tag,map (\((tag,incr),n) -> (incr,n)) xs)
                    [] -> error "pullTags given empty list"
    grouped0 = map pullTags $ List.groupBy (\((t0,_),_) ((t1,_),_) -> t0==t1) incrNodes0
    grouped1 = map pullTags $ List.groupBy (\((t0,_),_) ((t1,_),_) -> t0==t1) incrNodes1

    zippedByIncrs = [ (tag0,zip (map snd $ List.sortOn fst incrs0)
                                (map snd $ List.sortOn fst incrs1))
                    | (tag0,incrs0) <- grouped0, (tag1,incrs1) <- grouped1, tag0 == tag1 ]
  in noIncrZipped ++ concatMap snd zippedByIncrs


-- | Unions a list of tagged nodes by their tag. Uses @Data.List.unionBy@ and is
-- left-biased
unionByTag :: [(String,Node)] -> [(String,Node)] -> [(String, Node)]
unionByTag taggedNodes0 taggedNodes1 =
  List.unionBy (\n0 n1 ->
                  (takeWhile (/=':') $ fst n0 ) == (takeWhile (/=':') $ fst n1))
               taggedNodes0 taggedNodes1
