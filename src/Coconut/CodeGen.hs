-- |
-- Module      :  Coconut.CodeGen
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a functionality for taking Coconut DSL's and generating
-- assembly code

{-# LANGUAGE TupleSections,ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
module Coconut.CodeGen where

import Data.Word (Word64)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap as IM
import qualified Data.List as List
import qualified Data.Set as Set
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Bifunctor (bimap, second)
import Data.Either (rights)
import GHC.Unicode (toUpper)
import qualified Numeric
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS (topsort)
import Data.Graph.Inductive.Query.Dominators (dom)

import Coconut.BaseTypes
import Coconut.Graph.CodeGraph
import Coconut.Graph.DataFlow
import Coconut.Graph.ControlFlow
import Coconut.Schedule
import Coconut.RegisterAllocator

-- FIXME remove Core imports from CodeGen (generalize) or seperate CodeGen functionality from Core
-- import Coconut.Core.CoreISA
-- import Coconut.Core.Interp
-- import Coconut.Core.CodeGraph
-- -- import Coconut.Core.CoreHardware
-- import Coconut.Core.MetaData
-- import Coconut.Utils.CGWrappers (basicBlock)

import Debug.Trace

-- | Generate assembly code (in @ByteString@ format) from a scheduled @CodeGraph@.
--   Use @BS.writeFile@ to write the output to a file
--   Example Usage:
--   >> import Coconut.Schedule (defaultSchedule)
--   >> import Coconut.Core.MetaData (coreMetaData)
--   >> let schedule = defaultSchedule cg
--   >> let regMap = regAllocateCG schedule cg
--   >> codeGenCG cg schedule regMap coreMetaData
codeGenCG :: forall h . (Hardware h,Eq (RegType h),Printer h)
        => CodeGraph h
        -> (RegMap h,SpillMap,ScheduledGraph h)
        -> MDMap h
        -> ([ByteString],[ByteString])
codeGenCG cg (regMap,spillMap,scheduledGraph) metaData =
  let
    (CodeGraph cfGraph dfGraphs cgIn cgOut mrTables cTable tags maxID dbgMap) = cg

    -- create a subgraph of a ScheduledGraph that contains only the nodes in a given DataFlowGraph
    -- i.e. each corresponding to a single labelled code block
    dfGraphToSchedGraph :: DataFlowGraph h -> ScheduledGraph h
    dfGraphToSchedGraph (DataFlowGraph dfGraph _ _ _ _ _) =
      labnfilter (\(n,lnode) -> n `elem` map fst (labNodes dfGraph)
                             && not (snGlobalConst lnode)) scheduledGraph

    -- a list of code blocks (label,corresponding dataflow graph, corresponding scheduled graph)
    scheduledBlocks :: [(String,DataFlowGraph h,ScheduledGraph h)]
    scheduledBlocks = map (\(label,dfGraph) -> (label,dfGraph,dfGraphToSchedGraph dfGraph)) dfGraphs

    -- find all global constants (they are tagged in the scheduled graph)
    globalConstNodes = map fst $ filter (snGlobalConst . snd) $ labNodes scheduledGraph
    lookupDFGraph n = case matchNodeInCG cg n of
              CGDataFlowNode (lbl,dfNode) ->
                let
                  dfGraph = fromJust $ List.lookup lbl dfGraphs
                in (n,dfGraph)
              _ -> error $ "codeGenCG.lookupDFGraph globalConst isn't dataflow node "
                          ++ show n
    globalConsts = map lookupDFGraph globalConstNodes

    -- order all code blocks
    cfDoms = dom (controlFlowGraph cfGraph) cgIn
    _1 (a,_,_) = a
    orderedCFG = orderedCFGLabels cfGraph cfDoms cgIn $ map _1 scheduledBlocks
    orderedScheduleBlocks =
      sortByLabel (orderedCFGLabels cfGraph cfDoms cgIn $ map _1 scheduledBlocks) scheduledBlocks
    -- NOTE regMap here is updated by globalConstantsGenBlock
    allCodeBlocks = concatMap (codeGenBlock cg (regMap,spillMap) metaData) orderedScheduleBlocks

    -- header (including loading MR/constant table reg's)
    mrNodes = map (\(_,n,_) -> n) mrTables
    initMRSchedGraph = nfilter (`elem` mrNodes) scheduledGraph
    memLoadsBlock = memLoadsGenBlock mrTables cTable initMRSchedGraph regMap metaData
    globalConstBlock = globalConstantsGenBlock cg (regMap,spillMap) metaData globalConsts

    -- constant and MR tables
    cTableBlock = printableTable cg "CONSTANTS" cTable
    mrTableBlocks = concatMap ((uncurry (printableTable cg) . second (map runInterpVR))
                               . (\(label,_,tbl) -> (label,tbl)))
                    $ filter (\(lbl,_,_) -> lbl /= "SCRATCH") mrTables -- NOTE don't print scratch table since on stack
    allTableBlocks = cTableBlock ++ mrTableBlocks

    -- allBlocks = memLoadsBlock ++ allCodeBlocks ++ allTableBlocks
    -- put everything together
  in (memLoadsBlock++globalConstBlock++allCodeBlocks,allTableBlocks)
     `debug`
       ( "\ncodeGenCG dfGraphs: " ++ show (map fst dfGraphs)
       ++"\ncodeGenCG scheduledBlocks: " ++ show (map (\(lbl,_,_) -> lbl) scheduledBlocks)
       ++"\ncodeGenCG orderedScheduledBlocks: " ++ show (map (\(lbl,_,_) -> lbl) orderedScheduleBlocks)
       ++"\ncodeGenCG cfDoms: " ++ show cfDoms
       )

-- | Find all leftover registers, and create a block of loads that will serve as global constants while
-- rewriting the @RegMap@ to reflect this change (and returning a list of deleted instruction load Nodes)
globalConstantsGenBlock :: forall h . (Hardware h,Printer h)
                        => CodeGraph h
                        -> (RegMap h,SpillMap)
                        -> MDMap h
                        -> [(Node,DataFlowGraph h)]
                        -> [ByteString]
globalConstantsGenBlock cg (regMap,spillMap) metaData newConstLoads =
  let
    (CodeGraph cfGraph dfGraphs cgIn cgOut mrTables cTable tags maxID dbgMap) = cg

    -- allocatedRegs = map snd $ Map.toList regMap
    -- allRegs = concat [ map (\b -> (rt, b)) (regColors rt) | rt <- allRegTypes @h ]
    -- leftOverRegs = filter (\r -> not (r `elem` allocatedRegs)) allRegs

    -- allConstantLoads = concatMap (\(_,dfGraph) ->
    --                                 map (\(n,dfNode) -> (n,dfNode,dfGraph,head $ suc (dataFlowGraph dfGraph) n))
    --                                 $ filter (\(n,_) -> isConstantLoad dfGraph n)
    --                                 $ labNodes
    --                                 $ dataFlowGraph dfGraph) dfGraphs
    -- -- zip all constants (immediates represented by [Int]) with the instruction Node, the instruction DFNode,
    -- -- and the register Node respectively
    -- allConstLoadsWithImms = map (\(n,dfNode,dfGraph,n') ->
    --                                   case dfNode of
    --                                     InstructionNode (Instruction imms _ _) -> (imms,(n,dfNode,dfGraph,n'))
    --                                     _ -> error $ "node filtered by isConstantLoad isn't " ++ show n
    --                                ) allConstantLoads

    -- -- assign length leftOverRegs most used constants  (which are identified by [Int]) to new registers
    -- constRegAssigns =
    --   concatMap
    --              (\rt ->
    --                  let
    --                    leftOverRegs' = filter (\(rt',_) -> rt == rt') leftOverRegs
    --                    allConstLoadsWithImms' = filter (\(_,(_,_,_,n)) ->
    --                                                       fst (regMapLookup n regMap) == rt
    --                                                    ) allConstLoadsWithImms
    --                  in globalConstantsGenBlockByRegType cg regMap rt leftOverRegs' allConstLoadsWithImms')
    --              (allRegTypes @h)

    -- unpair (x,y) = [x,y]
    -- nodeIsOverwrite n = n `elem`
    --                       (concatMap (unpair . snd) $ concat (map (dataFlowOverwrites . snd) dfGraphs))

    -- nodeRegAssigns = concatMap assignRegByImm allConstLoadsWithImms
    -- assignRegByImm (imm,(n,dfNode,dfGraph,n'))
    --   | nodeIsOverwrite n' = []
    --   | otherwise = case List.lookup imm constRegAssigns of
    --                   Just (rt,bs) -> [((imm,n,dfNode,dfGraph,n'),(rt,bs))]
    --                   Nothing -> []

    -- regMap' = foldr (\((imm,n,dfNode,dfGraph,n'),(rt,bs)) rMap
    --                  -> Map.insert n' (rt,bs) rMap) regMap nodeRegAssigns
    -- allRemovedConstLoads = map (\((imm,n,dfNode,dfGraph,n'),(rt,bs)) -> (imm,n,dfGraph)) nodeRegAssigns

    -- newConstLoads = -- map (\(_,n,dfGraph) -> (n,dfGraph))
    --                  List.nubBy (\(imm0,_,_) (imm1,_,_) -> imm0 == imm1) allRemovedConstLoads

    addIndent bs = "    " <> bs
    globalConstLoadBlock = map addIndent
      $ genInstructions $ map (\(n,dfGraph) -> (n,dfGraph)) newConstLoads
                           `debug` ("newConstLoads " ++ show (map fst newConstLoads))

    -- NOTE this code is (almost) the same code used in codeGenBlock, should find some way to generalize this
    genInstructions :: [(Node,DataFlowGraph h)] -> [ByteString]
    genInstructions [] = []
    genInstructions ((n,dfGraph):nodes) =
      case matchInstrAndRegs dfGraph n of
        (Nothing,_,_,_,_) -> genInstructions nodes -- Skip non-format nodes, e.g. initMRs, Meets, Joins
        (format,name,args,imms,label) -> printableInstruction metaData format name args imms label (cgConstTable cg)
                                         : genInstructions nodes

    matchInstrAndRegs :: DataFlowGraph h
                      -> Node
                      -> (Maybe (InstructionFormat h),ByteString,[(RegType h,ByteString )],[Int],Maybe String)
    matchInstrAndRegs dfGraph n = case match n (dataFlowGraph dfGraph) of
      (Just (prevs,_,(InstructionNode el@(Instruction {})),succs),graph') ->
        let
          inRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) prevs
          outRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) succs
          allRegs = inRegs ++ outRegs
          instrName = case Map.lookup (elName el) (mdMap metaData) of
                        Just mData -> printableInstrLabel mData
                        Nothing -> error $ "missing instruction in metaData: " ++ elName el
          instrFormat = case Map.lookup (elName el) (mdMap metaData) of
                        Just mData -> Just (printableInstrFormat mData (elImmediates el))
                        Nothing -> error $ "missing instruction in metaData: " ++ elName el
        in (instrFormat,instrName,allRegs,elImmediates el,Nothing)
      (Just (prevs,_,(InstructionNode (Spill name)),succs),graph') ->
        let
          inRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) prevs
          outRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) succs
          spillNode = case succs of
                       [(_, n)] -> n
                       _ -> error "Spill has multiple outRegs"
          allRegs = inRegs ++ outRegs
          instrName = case Map.lookup name (mdMap metaData) of
                        Just mData -> printableInstrLabel mData
                        Nothing -> error $ "missing instruction in metaData: " ++ name
          instrFormat = case Map.lookup name (mdMap metaData) of
                        Just mData -> Just (printableInstrFormat mData [])
                        Nothing -> error $ "missing instruction in metaData: " ++ name
          spillOffset = case Map.lookup spillNode spillMap of
                          Just offset -> offset
                          Nothing -> error $ "missing spillNode " ++ show spillNode
        in (instrFormat,instrName,allRegs,[spillOffset],Just "FIFOS")
      (Just (prevs,_,(InstructionNode (Despill name)),succs),graph') ->
        let
          inRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) prevs
          outRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) succs
          allRegs = inRegs ++ outRegs
          spillNode = case prevs of
                       [(_,n)] -> n
                       _ -> error "Spill has multiple outRegs"
          instrName = case Map.lookup name (mdMap metaData) of
                        Just mData -> printableInstrLabel mData
                        Nothing -> error $ "missing instruction in metaData: " ++ name
          instrFormat = case Map.lookup name (mdMap metaData) of
                        Just mData -> Just (printableInstrFormat mData [])
                        Nothing -> error $ "missing instruction in metaData: " ++ name
          spillOffset = case Map.lookup spillNode spillMap of
                          Just offset -> offset
                          Nothing -> error $ "missing spillNode " ++ show spillNode
        in (instrFormat,instrName,allRegs,[spillOffset],Just "FIFOS")
      -- (Just (prevs,_,(InstructionNode (ScratchStore imms name)),succs),graph') ->
      --   let
      --     inRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) prevs
      --     outRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) succs
      --     allRegs = inRegs ++ outRegs
      --     instrName = case Map.lookup name (mdMap metaData) of
      --                   Just mData -> printableInstrLabel mData
      --                   Nothing -> error $ "missing instruction in metaData: " ++ name
      --     instrFormat = case Map.lookup name (mdMap metaData) of
      --                   Just mData -> Just (printableInstrFormat mData imms)
      --                   Nothing -> error $ "missing instruction in metaData: " ++ name
      --   in (instrFormat,instrName,allRegs,imms,Just "SCRATCH")
      -- (Just (prevs,_,(InstructionNode (ScratchLoad imms name)),succs),graph') ->
      --   let
      --     inRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) prevs
      --     outRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) succs
      --     allRegs = inRegs ++ outRegs
      --     instrName = case Map.lookup name (mdMap metaData) of
      --                   Just mData -> printableInstrLabel mData
      --                   Nothing -> error $ "missing instruction in metaData: " ++ name
      --     instrFormat = case Map.lookup name (mdMap metaData) of
      --                   Just mData -> Just (printableInstrFormat mData imms)
      --                   Nothing -> error $ "missing instruction in metaData: " ++ name
      --   in (instrFormat,instrName,allRegs,imms,Just "SCRATCH")
      (Just (prevs,_,(InstructionNode (Move name _)),succs),graph') ->
        let
          inRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) prevs
          outRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) succs
          allRegs = inRegs ++ outRegs
          instrName = case Map.lookup name (mdMap metaData) of
                        Just mData -> printableInstrLabel mData
                        Nothing -> error $ "missing instruction in metaData: " ++ name
          instrFormat = case Map.lookup name (mdMap metaData) of
                        Just mData -> Just (printableInstrFormat mData [])
                        Nothing -> error $ "missing instruction in metaData: " ++ name
        in (instrFormat,instrName,allRegs,[],Nothing)
      (Just (prevs,_,InstructionNode el,succs),graph') ->
        (Nothing, "skip", [], [],Nothing)
      (Just (prevs,dfNode,BranchNode instrName imm _,succs),graph') ->
        let
          inRegs = map (\(_,n) -> fromJust $ Map.lookup n regMap) prevs
          label = findBranchNodeLabel cg dfNode
          instrFormat = case Map.lookup instrName (mdMap metaData) of
                        Just mData -> Just (printableInstrFormat mData imm)
                        Nothing -> error $ "missing branch instruction in metaData: " ++ instrName
        in (instrFormat, BS.pack instrName,inRegs, imm,Just label)
      (Just (prevs,_,ResourceNode _,succs),graph') ->
        error $ "codeGEnBlock.matchInstrAndRegs ResourceNode encountered on node: " ++ show n
      (Nothing,_) -> error $ "codeGenBlock.matchInstrAndRegs bad match on node: " ++ show n

  in globalConstLoadBlock

globalConstantsGenBlockByRegType :: forall h . (Hardware h,Printer h)
                                 => CodeGraph h
                                 -> RegMap h
                                 -> RegType h
                                 -> [(RegType h,ByteString)]
                                 -> [([Int],(Node,DFNode h,DataFlowGraph h,Node))]
                                 -> [([Int],(RegType h,ByteString))]
globalConstantsGenBlockByRegType cg regMap regType leftOverRegs allConstLoadsWithImms =
  let
    numLeftOversRegs = length leftOverRegs
     -- get list of constants used sorted from most to least
    constCounts = (reverse . List.sortOn snd)
                  $ map (\grp -> (fst $ head grp, length grp))
                  -- List.groupBy (\(imms0,_) (imms1,_) -> imms0 == imms1) allConstLoadsWithImms
                  $ groupByImms allConstLoadsWithImms
    groupByImms [] = []
    groupByImms ((imm,n):ns) = ((imm,n) : [ (imm0,n0) | (imm0,n0) <- ns, imm0 == imm])
                               : (groupByImms $ filter (\(imm0,_) -> imm0 /= imm) ns)
    candidateConstants = zip (map fst constCounts) leftOverRegs

  in candidateConstants

memLoadsGenBlock :: forall h . (Hardware h,Printer h)
  => [(String,Node,[Interp VR])]
  -> [(Word64,Word64)]
  -> ScheduledGraph h
  -> RegMap h
  -> MDMap h
  -> [ByteString]
memLoadsGenBlock mrTables cTable schedGraph regMap metaData =
  let
    memLoadNodes = labNodes schedGraph
    labelMap = IM.fromList $ map (\(label,n,_) -> (n,label)) mrTables

    name = case Map.lookup "initMR" (mdMap metaData) of
               Just mData -> printableInstrLabel mData
               Nothing -> error "initMR missing from metaData"
    format = case Map.lookup "initMR" (mdMap metaData) of
               Just mData -> Just $ printableInstrFormat mData []
               Nothing -> error "initMR missing from metaData"
    memLoadCode = map (\(n,sNode) -> let
                            outNodes = snDefs sNode
                            outRegs = map (\(n,_) -> regMapLookup n regMap) outNodes
                            lbl = IM.lookup n labelMap
                          in printableInstruction metaData format name outRegs [] lbl cTable) memLoadNodes
    constLoadCode = printableInstruction metaData format name [constantReg] [] (Just "CONSTANTS") cTable
    addIndent bs = "    " <> bs
  in map addIndent $ constLoadCode:memLoadCode

orderedCFGLabels :: ControlFlowGraph h
                -> [(Node,[Node])] -- each element of the list is a node and its dominators
                -> Node
                -> [String]
                -> [String]
orderedCFGLabels cfg cfDoms node basicBlocks = let
    (_,nodeID,label,outEdges) = context (controlFlowGraph cfg) node
    in concatMapNub [] (orderedCFGLabels' cfg cfDoms node basicBlocks) $ List.sortBy compareJumpEdges outEdges

concatMapNub list0 f = foldl
      (\ list0 list -> list0 ++ ([x | x <- f list, x `notElem` list0]))
      list0

orderedCFGLabels' :: ControlFlowGraph h
                -> [(Node,[Node])]
                -> Node
                -> [String]
                -> (CFEdge h, Node)
                -> [String]
orderedCFGLabels' cfg cfDoms inNode basicBlocks (cfEdge, outNode) = case cfEdge of
  CFDataFlow dataFlowLabel ->
    -- NOTE check if outNode is a dominator of inNode, if so don't recurse
    case elem outNode <$> lookup inNode cfDoms of
      Just True  -> [dataFlowLabel]
      _ -> dataFlowLabel:(orderedCFGLabels cfg cfDoms outNode basicBlocks)
  _ ->
    case elem outNode <$> lookup inNode cfDoms of
      Just True -> []
      _ -> orderedCFGLabels cfg cfDoms outNode basicBlocks

sortByLabel
  :: forall h
   . [String]
  -> [(String,DataFlowGraph h,ScheduledGraph h)]
  -> [(String,DataFlowGraph h,ScheduledGraph h)]
sortByLabel sortedLabels basicBlocks = map extractLabel sortedLabels
  where
    extractLabel :: String -> (String,DataFlowGraph h,ScheduledGraph h)
    extractLabel label =
      case lookup label $ map threeTupleToTwoTuple basicBlocks of
        Nothing -> error $ "missing label in sortByLabel: " ++ label
        Just (dfg, schg) -> (label, dfg, schg)

compareJumpEdges :: (CFEdge h, a) -> (CFEdge h, a) -> Ordering
compareJumpEdges (CFBranchNE _ _, _) (CFBranchEQ _ _, _) = LT
compareJumpEdges _ _ = GT

threeTupleToTwoTuple :: (a,b,c) -> (a,(b,c))
threeTupleToTwoTuple (a,b,c) = (a,(b,c))

twoTupleToThreeTuple :: (a,(b,c)) -> (a,b,c)
twoTupleToThreeTuple (a,(b,c)) = (a,b,c)

lookupLabel :: Eq a => a -> [(a,b,c)] -> Maybe (a,b,c)
lookupLabel label xs = case lookup label $ map threeTupleToTwoTuple xs of
  Nothing -> Nothing
  Just (b,c) -> Just (label,b,c)

-- | Generates ByteStrings corresponding to as single basic block of assembly code
codeGenBlock :: forall h . (Hardware h,Eq (RegType h),Printer h)
  => CodeGraph h
  -> (RegMap h,SpillMap)
  -> MDMap h
  -> (String,DataFlowGraph h,ScheduledGraph h) -> [ByteString]
codeGenBlock cg (regMap,spillMap) metaData (label,dfGraph,schedGraph) =
  let
    blockHeader :: ByteString
    blockHeader = printableSectionLabel cg label

    -- all lines of assembly code for a basic block should be indented (not including the header)
    indent :: ByteString -> ByteString
    indent line = "    " `BS.append` line

    -- if block is a loops to itself, need to delete the loop edge
    isBranch n = case match n (dataFlowGraph dfGraph) of
       (Just (prevs,_,BranchNode {},succs),graph') -> True
       _ -> False
    isLoopingEdge (n0,n1) = isBranch n0 && n1 `elem` nodes schedGraph
    loopingEdges = filter isLoopingEdge $ edges schedGraph
    schedGraph' = delEdges loopingEdges schedGraph

    -- schedGraph is a ScheduledGraph containing only instructions connected in
    -- order according to how they were scheduled, is completely linear because
    -- its a basic block, so its topological sort gives a list of instruction
    -- nodes in correct schedule order
    scheduledNodes :: [Node]
    scheduledNodes = -- filter (\n -> not (n `elem` removedConstLoads)) $
                     topsort schedGraph'

    -- all the assembly code for the basic block minus the header
    blockCode :: [ByteString]
    blockCode = genInstructions scheduledNodes

    -- use's Printer class method printableInstruction to format
    genInstructions :: [Node] -> [ByteString]
    genInstructions [] = []
    genInstructions (n:nodes) =
      case matchInstrAndRegs n of
        (Nothing,_,_,_,_) -> genInstructions nodes -- Skip non-format nodes, e.g. initMRs, Meets, Joins
        (format,name,args,imms,label) -> printableInstruction metaData format name args imms label (cgConstTable cg) : genInstructions nodes

    -- because MDMap is an associated type, we can't access its contents correctly
    -- use's Printer class method printableInstrLabel to look up instruction label in metaData
    matchInstrAndRegs :: Node -> (Maybe (InstructionFormat h),ByteString,[(RegType h,ByteString )],[Int],Maybe String)
    matchInstrAndRegs n = case match n (dataFlowGraph dfGraph) of
      (Just (prevs,_,(InstructionNode el@(Instruction {})),succs),graph') ->
        let
          inRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) prevs
          outRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) succs
          allRegs = inRegs ++ outRegs
          instrName = case Map.lookup (elName el) (mdMap metaData) of
                        Just mData -> printableInstrLabel mData
                        Nothing -> error $ "missing instruction in metaData: " ++ elName el
          instrFormat = case Map.lookup (elName el) (mdMap metaData) of
                        Just mData -> Just (printableInstrFormat mData (elImmediates el))
                        Nothing -> error $ "missing instruction in metaData: " ++ elName el
        in (instrFormat,instrName,allRegs,elImmediates el,Nothing)
      (Just (prevs,_,(InstructionNode (Spill name)),succs),graph') ->
        let
          inRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) prevs
          outRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) succs
          spillNode = case succs of
                       [(_, n)] -> n
                       _ -> error "Spill has multiple outRegs"
          allRegs = inRegs ++ outRegs
          instrName = case Map.lookup name (mdMap metaData) of
                        Just mData -> printableInstrLabel mData
                        Nothing -> error $ "missing instruction in metaData: " ++ name
          instrFormat = case Map.lookup name (mdMap metaData) of
                        Just mData -> Just (printableInstrFormat mData [])
                        Nothing -> error $ "missing instruction in metaData: " ++ name
          spillOffset = case Map.lookup spillNode spillMap of
                          Just offset -> offset
                          Nothing -> error $ "missing spillNode " ++ show spillNode
        in (instrFormat,instrName,allRegs,[spillOffset],Just "FIFOS")
      (Just (prevs,_,(InstructionNode (Despill name)),succs),graph') ->
        let
          inRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) prevs
          outRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) succs
          allRegs = inRegs ++ outRegs
          spillNode = case prevs of
                       [(_,n)] -> n
                       _ -> error "Spill has multiple outRegs"
          instrName = case Map.lookup name (mdMap metaData) of
                        Just mData -> printableInstrLabel mData
                        Nothing -> error $ "missing instruction in metaData: " ++ name
          instrFormat = case Map.lookup name (mdMap metaData) of
                        Just mData -> Just (printableInstrFormat mData [])
                        Nothing -> error $ "missing instruction in metaData: " ++ name
          spillOffset = case Map.lookup spillNode spillMap of
                          Just offset -> offset
                          Nothing -> error $ "missing spillNode " ++ show spillNode
        in (instrFormat,instrName,allRegs,[spillOffset],Just "FIFOS")
      (Just (prevs,_,(InstructionNode (Move name _)),succs),graph') ->
        let
          inRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) prevs
          outRegs = map (\(_,n) -> regMapLookup n regMap) $ List.sortOn (dfEdgeIndex . fst) succs
          allRegs = inRegs ++ outRegs
          instrName = case Map.lookup name (mdMap metaData) of
                        Just mData -> printableInstrLabel mData
                        Nothing -> error $ "missing instruction in metaData: " ++ name
          instrFormat = case Map.lookup name (mdMap metaData) of
                        Just mData -> Just (printableInstrFormat mData [])
                        Nothing -> error $ "missing instruction in metaData: " ++ name
        in (instrFormat,instrName,allRegs,[],Nothing)
      (Just (prevs,_,InstructionNode el,succs),graph') ->
        (Nothing, "skip", [], [],Nothing)
      (Just (prevs,dfNode,BranchNode name imm _,succs),graph') ->
        let
          inRegs = map (\(_,n) -> fromJust $ Map.lookup n regMap) prevs
          label = findBranchNodeLabel cg dfNode
          instrName = case Map.lookup name (mdMap metaData) of
                        Just mData -> printableInstrLabel mData
                        Nothing -> error $ "missing instruction in metaData: " ++ name
          instrFormat = case Map.lookup name (mdMap metaData) of
                        Just mData -> Just (printableInstrFormat mData imm)
                        Nothing -> error $ "missing branch instruction in metaData: " ++ name
        in (instrFormat, instrName,inRegs, imm,Just label)
      (Just (prevs,_,ResourceNode _,succs),graph') ->
        error $ "codeGEnBlock.matchInstrAndRegs ResourceNode encountered on node: " ++ show n
      (Nothing,_) -> error $ "codeGenBlock.matchInstrAndRegs bad match on node: " ++ show n

  in [blockHeader] ++ map indent blockCode


findBranchNodeLabel cg dfNode =
  let
    cfGraph = cgControlFlow cg
    cfNodes = labNodes $ controlFlowGraph cfGraph
    possibleCFNodes = concatMap (\(n0,lCFNode ) -> case lCFNode of
                                     CFBranchNode (_,n) _ -> if n == dfNode
                                                               then [n0]
                                                               else []
                                     _ -> []) cfNodes
    cfNode = case possibleCFNodes of
               (n:_) -> n
               _ -> error $ "findBranchNodeLabel couldn't find any CFNodes containing: " ++ show dfNode
    findLabels cfEdges =  concatMap (\cfEdge -> case cfEdge of
                             CFBranchEQ label _ -> [label]
                             CFJump (_,label) _ -> [label]
                             _ -> [])  cfEdges
  in case match cfNode (controlFlowGraph $ cgControlFlow cg) of
    (Just (prevs,_,CFBranchNode _ _,succs),_) ->
      case findLabels (map fst succs) of
        (label:_) -> label
        _ -> error "finBranchNodeLabel CFBranchNode has no CFBranchEQ edge"
    _ -> error "findBranchNodeLabel not given CFBRanchNode"

-- codeGenConstTable :: String -> [(Word64, Word64)] -> [ByteString]
-- codeGenConstTable label pairs = let
--   header = BS.pack label <> "   DS   0L"
--   genPair (a,b) = ["    DC    XL8'" <> BS.pack (Numeric.showHex a "") <> "'", "    DC    XL8'" <> BS.pack (Numeric.showHex b "") <> "'"]
--   in header : concatMap genPair pairs

-- regMapLookup n regMap = case Map.lookup n regMap of
--                           Just r -> r
--                           Nothing -> error $ "Bad lookup on node: " ++ show n
--                                     ++ "\nAnd regMap: " ++ show regMap
-- Test code

-- testBlockB :: forall repr . CoreISA repr => (repr GPR,repr GPR) -> repr GPR
-- testBlockB (g0,g1) =
--   let
--     g2 = addG g0 g1
--     g3 = addG g2 g1
--   in g3

-- testBasicBlockB :: CodeGraph CORE
-- testBasicBlockB =
--   let
--     inTags = ["g0","g1"]
--     outTags = ["g3"]
--     bBlock = basicBlock "bBlock" inTags outTags (testBlockB @(Coconut.Graph.CodeGraph.Graph CORE))
--    in createCG (genBlock bBlock)

-- testScheduleGraph :: ScheduledGraph CORE
-- testScheduleGraph =
--   let
--     schedule = defaultSchedule testBasicBlockB
--   in buildScheduledGraph schedule testBasicBlockB

-- -- testInterferenceGraph :: (IntMap FGL.Node, InterferenceGraph CORE)
-- testInterferenceGraph = let
--     schedule = defaultSchedule testBasicBlockB
--   in genInterferenceGraph testBasicBlockB schedule

-- instance Show (ResType h) => (Show (RegNode h)) where
--   show (RegNode resType) = "RegNode (" ++ show resType ++ ")"

-- testRegAllocateCG :: Either [Node] (RegMap CORE)
-- testRegAllocateCG =
--   let
--     schedule = defaultSchedule testBasicBlockB
--   in regAllocateCG schedule testBasicBlockB

-- testCodeGen =
--   let
--     schedule = defaultSchedule testBasicBlockB
--     regMap = case regAllocateCG schedule testBasicBlockB of
--                Left nodes -> error $ "RegAllocation failed on nodes " ++ show nodes
--                Right rMap -> rMap
--   in codeGenCG testBasicBlockB schedule regMap coreMetaData
