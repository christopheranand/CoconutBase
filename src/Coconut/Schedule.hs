-- |
-- Module      :  Coconut.Scheduler
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a functionality for scheduling coconut code

{-# LANGUAGE RankNTypes,ScopedTypeVariables,FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module Coconut.Schedule where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (fromJust,isJust)
import Data.List (nub,partition)
import qualified Data.List as List
import Data.List.Split
import Data.Char (toUpper)
import System.Random
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Data.ByteString.Char8 (ByteString)
import Data.Graph.Inductive.Graph hiding (Graph)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS (topsort)


import HashedExpression hiding (Node)
import HashedExpression.Modeling.Typed

import Coconut.BaseTypes
import Coconut.Graph.CodeGraph
import Coconut.Graph.ControlFlow
import Coconut.Graph.DataFlow -- (DataFlowGraph (..),DFNode(..),DFEdge(..),partGraphByInstr, tieNodesByTag)
import Coconut.Core.CoreISA
import Coconut.Core.ControlISA
import Coconut.Core.CodeGraph
import Coconut.Utils.CGWrappers
import Coconut.HashedSchedule
import Coconut.RegisterAllocator

-- import Coconut.Graph.TopSort
import Debug.Trace (trace,traceM)
import System.Process
import Coconut.Rewrite (cgApplyRewrites)
import Data.Primitive (newAlignedPinnedByteArray)


-- | Performas scheduling then register allocation. If register allocation
-- fails, add a spill to the CodeGraph and reschedule. Repeat until register
-- alllocation suceeds
scheduleAndRegAllocate :: Hardware h =>
  CodeGraph h
  -> Int
  -> MDMap h
  -> [HashedPenalty h]
  -> [HashedConstraint h]
  -> Maybe StdGen
  -> Bool
  -> Solver
  -> Int
  -> [(String,ByteString)]
  -> IO (CodeGraph h,Schedule,RegMap h,SpillMap,ScheduledGraph h,Double,StdGen)
scheduleAndRegAllocate cg unroll metaData penalties constraints stdgen0 reuse solver numStages fixedRegs =
  do (cg',sched,ii,stdgen,hashedData) <- genHashedSchedule cg unroll metaData penalties constraints stdgen0 reuse solver numStages
     let eRegMap = regAllocateCG sched cg' fixedRegs
     case eRegMap of
       Right (rMap,sMap,sGraph) -> do
         putStrLn $ "\nSpillMap after regAllocate: " ++ show sMap ++ "\n"
         return (cg',sched,rMap,sMap,sGraph,ii,stdgen)
       Left (failedNodes,liveSets) ->
         let
          spills0 = failedSpillCandidates hashedData cg' failedNodes liveSets
          spillsD = defaultSpillCandidates hashedData
          -- failedSpillCandidates might not return a spill for every failed regType
          -- in this case, use defaultSpillCandidate instead
          spills = map (\(rt,mN) -> case mN of
                           Just n -> (rt,n)
                                    `debug` ("failedSpill selected: " ++ show n)
                           Nothing -> case filter (\(rt',_) -> rt == rt') spillsD of
                                        (n:_) -> n
                                                 `debug` ("defaultSpill selected: " ++ show n)
                                        [] -> error $ "regType missing from default spills: "
                                              ++ show (rt,spillsD)
                           ) spills0
          -- only keep spills of register types that failed
          cgN = addSpill cg spills
         in do putStrLn $ "Reg Alloc failed on nodes: " ++ show failedNodes
                        ++ "\nadding spills: " ++ show spills
               stdgen' <- newStdGen
               -- NOTE don't add spills, just generate new random schedule
               scheduleAndRegAllocate cg unroll metaData penalties constraints (Just stdgen') reuse solver numStages fixedRegs
               -- scheduleAndRegAllocate cgN unroll metaData penalties constraints stdgen0 reuse solver numStages fixedRegs

scheduleAndRegAllocateDbg cg unroll metaData penalties constraints stdgen0 reuse solver numStages fixedRegs =
  do (cg',sched,ii,stdgen,hashedData) <- genHashedSchedule cg unroll metaData penalties constraints stdgen0 reuse solver numStages
     let eRegMap = regAllocateCG sched cg' fixedRegs
     case eRegMap of
       Right (rMap,sMap,sGraph) -> do
         putStrLn $ "\nReg Alloc Success with spillMap: " ++ show sMap ++ "\n"
         return cg'
       Left (failedNodes,liveSets) ->
         let
          spills0 = failedSpillCandidates hashedData cg' failedNodes liveSets
          spillsD = defaultSpillCandidates hashedData
          -- failedSpillCandidates might not return a spill for every failed regType
          -- in this case, use defaultSpillCandidate instead
          spills = map (\(rt,mN) -> case mN of
                           Just n -> (rt,n)
                                    `debug` ("failedSpill selected: " ++ show n)
                           Nothing -> case filter (\(rt',_) -> rt == rt') spillsD of
                                        (n:_) -> n
                                                 `debug` ("defaultSpill selected: " ++ show n)
                                        [] -> error $ "regType missing from default spills: "
                                              ++ show (rt,spillsD)
                           ) spills0
          -- only keep spills of register types that failed
          cgN = addSpill cg spills
         in do putStrLn $ "Reg Alloc failed on nodes: " ++ show failedNodes
                        ++ "\nadding spills: " ++ show spills
               return cgN
               --scheduleAndRegAllocate cgN unroll metaData penalties constraints stdgen0 reuse solver numStages fixedRegs

failedSpillCandidates :: forall h . Hardware h
                       => HashedData h
                       -> CodeGraph h
                       -> [(RegType h,Node,RegMap h)]
                       -> LivenessSets
                       -> [(RegType h,Maybe (Node,Node))]
failedSpillCandidates hashedData cg' failedNodes liveSets =
  let
    -- only create spills for regtypes of registers that have failed
    failedRegTypes = filter (\rt -> rt `elem` (map (\(f,_,_) -> f) failedNodes)) (allRegTypes @h)
  in map (\regType -> (regType,failedSpillCandidate hashedData cg' failedNodes regType liveSets))
     failedRegTypes
failedSpillCandidate hashedData cg' failedNodes regType liveSets =
  let
    dfGraph = dataFlowGraph $ hdDataFlow hashedData
    depthMap = hdDepthMap hashedData

    isRegType n = case matchNodeInCG cg' n of
                    CGDataFlowNode (_,ResourceNode (RegisterRes rt))  -> rt == regType
                    _ -> False

    allLiveSets :: [IntSet] -- Filter livesets to only contain regType
    allLiveSets = map (\intset ->
                         IntSet.filter isRegType intset) allLiveSets0

    allLiveSets0 = (map snd $ IntMap.toList $ livenessIn liveSets)
                   ++ (map snd $ IntMap.toList $ livenessOut liveSets)

    failedNode = case filter (\(rt,_,_) -> rt == regType) failedNodes of
                   ((_,n,_):_) -> n
                   _ -> error "failedSpillCandidate given bad regType"

    cmpLiveSets s0 s1 = if IntSet.size s0 < IntSet.size s1
                        then LT
                        else if IntSet.size s0 == IntSet.size s1
                                then EQ
                                else GT
    -- failedLiveSets = filter (\set -> IntSet.member failedNode set) allLiveSets
    greatestLiveSet = case reverse $  List.sortBy cmpLiveSets allLiveSets of
                        (lSet:_) -> lSet
                        _ -> error $ "no livesets: " ++ show failedNode
                                   ++ "\n" ++ show allLiveSets

    -- keep only nodes that can be spilled (or unmappable to the original codegraph)
    unMapNodes = map fromJust . filter isJust . map (unIncrementNode cg')
    greatestLiveSet' = unMapNodes $ IntSet.toList greatestLiveSet
                       `debug` ("Max Reg Pressure "++show regType++": "
                                ++(show $ IntSet.size greatestLiveSet))
    allLiveSets' = map unMapNodes $ map IntSet.toList allLiveSets

    -- find how many livesets a node is contained in
    numLiveSets n = length $ filter (\s -> n `elem` s) allLiveSets'
    liveMaximums = reverse $ List.sortOn snd $ map (\n -> (n,numLiveSets n)) greatestLiveSet'

    isSpill n = case match n dfGraph of
          (Just (_,_,InstructionNode el,_),_ ) ->
            case el of
                Spill _ -> True
                _ -> False
          _ -> False
    regIsAlreadySpilled n = or [isSpill s | s <- suc dfGraph n]

    regIsMemoryRes n = case match n dfGraph of
          (Just (_,_,ResourceNode rt,_),_ ) ->
            case rt of
                MemoryRes _ -> True
                _ -> False
          _ -> False

    isLoadH n = case match n dfGraph of
          (Just (_,_,dfNode,_),_ ) -> isHLoad dfNode
          _ -> False
    regIsProducedByLoad n = or [isLoadH s | s <- pre dfGraph n]
    regConsumedByStore n = or [isHStore $ fromJust $ lab dfGraph s | s <- suc dfGraph n]

    validCandidate c = (not $ regIsAlreadySpilled c)
                     && (not $ regIsMemoryRes c)
                     && (not $ regIsProducedByLoad c)
                     && (not $ regConsumedByStore c)

    -- remove mem nodes (TODO also remove overwrite nodes from spill candidates?)
    liveMaximums' = filter (validCandidate . fst) liveMaximums
                  -- ++ filter (regIsAlreadySpilled . fst) liveMaximums
    -- since we only spill/despill one consumer, prefer registers with smaller (ideally 1) consumer
    -- minConsumers = minimum $ map (\(n,_) -> length $ suc dfGraph n)
    --                        $ filter (\(n,_) -> (length $ suc dfGraph n) /= 0) liveMaximums'
    -- spillCandidate = case dropWhile (\(n,_)-> minConsumers /= (length $ suc dfGraph n)) liveMaximums' of
    spillCandidate = case liveMaximums' of
                       ((n,_):_) -> n
                       _ -> error "liveMaximums is empty"
    fNodeWithInstrs = (pre dfGraph spillCandidate,spillCandidate,suc dfGraph spillCandidate)

    -- REPLACE sort failed spilled candidates by maximum depth+height
    getLatencyLen :: ([Node],Node,[Node]) -> (Node,Int,Node)
    getLatencyLen (_,regN,[]) = (regN,0,error $ "failedSpill: reg with no consumer selected for spill: "++show regN)
    getLatencyLen (prods,regN,cons) =
      let
        minLat = foldr min 0 $ map (\n -> fromJust $ Map.lookup n depthMap) prods
        (maxLat,maxCon) = List.maximumBy (\(_,l0) (_,l1) -> compare l0 l1)
                          $ map (\n -> (fromJust $ Map.lookup n depthMap,n)) cons
      in (regN,maxLat - minLat,maxCon)

  in case suc dfGraph spillCandidate of
                   [] -> Nothing -- selected node doesn't have any successors?
                   _ -> case getLatencyLen fNodeWithInstrs of
                          (regN,_,maxCon) -> Just (regN,maxCon)


defaultSpillCandidates :: forall h . Hardware h => HashedData h -> [(RegType h,(Node,Node))]
defaultSpillCandidates hashedData = map (\regType -> (regType,defaultSpillCandidate hashedData regType))
                                    (allRegTypes @h)
defaultSpillCandidate hashedData regType =
  let
        dfGraph = dataFlowGraph $ hdDataFlow hashedData
        depthMap = hdDepthMap hashedData
        isRegType regType dfNode =
          case dfNode of
            ResourceNode (RegisterRes regType') -> regType == regType'
            _ -> False
        -- list of register node of regType
        regNodes0 = map fst
                   $ filter (\(_,dfNode) -> isRegType regType dfNode)
                   $ labNodes dfGraph

        isDespill n = case match n dfGraph of
              (Just (_,_,InstructionNode el,_),_ ) ->
                case el of
                    Despill _ -> True
                    _ -> False
              _ -> False

        regIsMemoryRes n = case match n dfGraph of
              (Just (_,_,ResourceNode rt,_),_ ) ->
                case rt of
                    MemoryRes _ -> True
                    _ -> False
              _ -> False
        regProducedByDespill n = or [isDespill s | s <- pre dfGraph n]
        regConsumedByStore n = or [isHStore $ fromJust $ lab dfGraph s | s <- suc dfGraph n]
        regNodes =  filter (\n -> not $ regProducedByDespill n
                                       || regConsumedByStore n
                                       || regIsMemoryRes n) regNodes0

        -- find all instructions that produce and consume the register
        regWithInstrs = map (\n -> (pre dfGraph n,n,suc dfGraph n)) regNodes
        -- compute the latency length of a register
        getLatencyLen (_,regN,[]) = (regN,0,error $ "defaultSpill: reg with no consumer selected for spill: "++show regN)
        getLatencyLen (prods,regN,cons) =
          let
            minLat = foldr min 0 $ map (\n -> fromJust $ Map.lookup n depthMap) prods
            (maxLat,maxCon) = List.maximumBy (\(_,l0) (_,l1) -> compare l0 l1)
                              $ map (\n -> (fromJust $ Map.lookup n depthMap,n)) cons
          in (regN,maxLat - minLat,maxCon)
      in case List.maximumBy (\(_,l0,_) (_,l1,_) -> compare l0 l1) $ map getLatencyLen regWithInstrs of
           (regN',_,maxCon') -> (regN',maxCon')

-- | Add a spill to the codegraph. Should take a list of failed nodes (with
-- their register type) returned from @chaitinBriggsAllocate@. For each fail
-- node (there should be one per register type that failed) a spill is added for
-- the corresponding register type
addSpill :: Hardware h => CodeGraph h -> [(RegType h, (Node, Node))] -> CodeGraph h
addSpill cg [] = cg
addSpill cg ((rType,(rNode,cNode)):spillNodes) =
  let
    (dfLabel,dfNode) = case matchNodeInCG cg rNode of
                            CGDataFlowNode d -> d
                            _ -> error $ "addSpill given bad spill candidate: " ++ show rNode
    dfGraph = fromJust $ List.lookup dfLabel $ cgDataFlows cg
    fglGraph = dataFlowGraph dfGraph

    rEdge = case match rNode fglGraph of
              (Just (_,_,_,cons),_) ->
                case filter (\(_,n) -> n == cNode) cons of
                  (e,_):_ -> e
                  _ -> error $ "addSpill given non-existent edge: " ++ show (rNode,cNode)
              _ -> error $ "addSpill given missing node: " ++ show rNode

    allConsumerNodes = suc (dataFlowGraph dfGraph) rNode

    -- TODO to support spills across basic block, we can't assume rNode and
    -- cNode are in the same dataflow graph

    maxID = cgMaxID cg
    spillRegID = maxID+1
    spillInstrID = maxID+2
    despillInstrID = maxID+3
    newRegID = maxID+4
    maxID' = newRegID

    spillReg = (spillRegID,ResourceNode SpillRes)
    spillInstr = (spillInstrID,InstructionNode (Spill $ spillName rType))
    deSpillInstr = (despillInstrID,InstructionNode (Despill $ despillName rType))
    newReg = (newRegID,dfNode)

    -- Need to convert (r) - (c)
    -- TO
    -- (r) - (spill) - (s) - (despill) - (r') - (c)
    fglGraph' = insEdge (rNode,spillInstrID,rEdge)
                $ insEdge (spillInstrID,spillRegID,DFEdge 0)
                $ insEdge (spillRegID,despillInstrID,DFEdge 0)
                $ insEdge (despillInstrID,newRegID,DFEdge 0)
                $ insEdge (newRegID,cNode,DFEdge 0)
                $ insNode deSpillInstr
                $ insNode spillInstr
                $ insNode spillReg
                $ insNode newReg
                $ delEdge (rNode,cNode) fglGraph

    insertConsumeSpills [] fglGraph0 = fglGraph0
    insertConsumeSpills (c:cs) fglGraph0 =
      let
        fglGraph0' = insEdge (newRegID,c,DFEdge 0)
                   $ delEdge (rNode,c) fglGraph0
      in insertConsumeSpills cs fglGraph0'

    -- NOTE replace dfGraph' with dfGraph0 to load spills on all consumer nodes
    dfGraph0 = dfGraph { dataFlowGraph = insertConsumeSpills (filter (/=cNode) allConsumerNodes) fglGraph' }

    dfGraph' = dfGraph { dataFlowGraph = fglGraph' }
    dataFlows' = (dfLabel,dfGraph') : (filter (\(lbl,_) -> lbl /= dfLabel) $ cgDataFlows cg)

  in cg { cgDataFlows = dataFlows'
        , cgMaxID = maxID' }


-- * Non-Modulo Scheduling

defaultSchedule :: forall h . Hardware h => CodeGraph h -> Schedule
defaultSchedule cg@(CodeGraph cfGraph dfGraphs cgIn cfOut mrTables cTable tags maxID dbgMap) =
  let

    scheduledNodes :: [Node]
    scheduledNodes = scheduleControlFlowGraph cg

  in Map.fromList $ zip scheduledNodes [0..]

scheduleControlFlowGraph :: forall h . Hardware h => CodeGraph h -> [Node]
scheduleControlFlowGraph cg =
  let
    cfGraph = cgControlFlow cg
    dfGraphs = cgDataFlows cg

    sortedCFNodes :: [Node]
    sortedCFNodes = topsort $ controlFlowGraph cfGraph

    cfNodePositions :: [(Node,Int)]
    cfNodePositions = zip sortedCFNodes [0..]

    onlyDFEdges :: [LEdge (CFEdge h)]
    onlyDFEdges = filter (\(_,_,e) -> case e of
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
    sortedDataFlows = concatMap (\(_,_,e) -> case e of
                                    CFDataFlow label -> case List.lookup label dfGraphs of
                                                          Just dfGraph -> [(label,dfGraph)]
                                                          Nothing -> error $ "Missing dfGraph: " ++ label
                                    _ -> []) sortedCFEdges

   in concatMap (scheduleDataFlowGraph) sortedDataFlows


scheduleDataFlowGraph :: Show (RegType h) => (String,DataFlowGraph h) -> [Node]
scheduleDataFlowGraph (label,DataFlowGraph dfGraph dfIns dfOuts overwrites incr stage) =
  let
    isInstruction :: Gr (DFNode h) DFEdge -> Node -> Bool
    isInstruction dfGraph n =
      case match n dfGraph of
        (Just (_,_,nodeLbl,_),_) -> case nodeLbl of
                                      InstructionNode _ -> True
                                      _ -> False
        (Nothing,_) -> False

    isBranch :: Gr (DFNode h) DFEdge -> Node -> Bool
    isBranch dfGraph n =
      case match n dfGraph of
        (Just (_,_,nodeLbl,_),_) -> case nodeLbl of
                                      BranchNode _ _ _ -> True
                                           -- `debug` ("matched branch in graph: " ++ label)
                                      _ -> False
        (Nothing,_) -> False

    -- TODO need to catch other ovewrites (like instructions that overwrite their inputs)
    hasOutputOverwrite node =
      case match node dfGraph of
        (Just (prevs,_,_,succs),_) -> hasInOutTie prevs succs
        _ -> False

    -- if an instruction takes in an input and outputs to the same tag and is in a loop,
    -- it has to be put in the same register
    -- this will cause a bad overwrite (write before read hazard) if another instruction needs to use that input
    hasInOutTie prevs succs = any (`elem` inOutTies) [ (p,s) | (_,p) <- prevs, (_,s) <- succs]
    mrLbls = ["mrIn","mrOut","scratchIn","scratchOut"]
    inOutTies = [ (n0,n1) | (s0,n0) <- filter (\(lbl,_) -> lbl `notElem` mrLbls) dfIns
                          , (s1,n1) <- filter (\(lbl,_) -> lbl `notElem` mrLbls) dfOuts
                          , s0 == s1]

    topSortedNodes :: [Node]
    topSortedNodes = topsort dfGraph

    sortedInstructions :: [Node]
    sortedInstructions = filter (not . hasOutputOverwrite) sortedInstructions0
                          ++ filter hasOutputOverwrite sortedInstructions0
    sortedInstructions0 = filter (isInstruction dfGraph) topSortedNodes


    branchNodes :: [Node]
    branchNodes = filter (isBranch dfGraph) topSortedNodes
    debugTrace = flip $ trace
  in sortedInstructions ++ branchNodes
     -- `debugTrace` (if label=="moduloB_LOOPEND"
     --               then show dfGraph
     --               else "")

-- * Modulo Scheduling

genHashedSchedule :: forall h . Hardware h
  => CodeGraph h
  -> Int
  -> MDMap h
  -> [HashedPenalty h]
  -> [HashedConstraint h ]
  -> Maybe StdGen
  -> Bool
  -> Solver
  -> Int
  -> IO (CodeGraph h,Map Node Double,Double,StdGen,HashedData h)
genHashedSchedule cg unroll metaData penalties constraints stdgen0 reuse solver numStages =
  let
    (CodeGraph cfGraph dfGraphs cgIn cfOut mrTables cTable tags maxID dbgMap) = cg

    edgeIsModulo (_,_,CFModuloBody _) = True
    edgeIsModulo _ = True

    moduloEdges = filter edgeIsModulo $ labEdges $ controlFlowGraph cfGraph
    moduloEdge = case moduloEdges of
       ((n0,n1,CFModuloBody label)):edges -> fmap (\d -> (label,d)) $ List.lookup label dfGraphs
       _ -> Nothing

    solverDir =
      case solver of
        Ipopt -> "ipopt"
        Glpk -> "glpk"
  in case moduloEdge of
       Just (label,dfGraph) -> do
          stdgen <- case stdgen0 of
                      Nothing ->
                        do putStrLn "\nGenerating a new random seed"
                           stdgen' <- newStdGen
                           return stdgen'
                      Just stdgen' -> return stdgen'
          hashedData <-
            if (not reuse)
               then (do putStrLn $ "\nGenerating " ++ solverDir ++ " Code"
                        sc <- moduloToHashedExpressionC cg metaData (label,dfGraph) numStages stdgen penalties constraints solver
                        putStrLn "\nRunning `make`"
                        readCreateProcess ((shell "make") {cwd = Just $ solverDir ++ "/src/" }) ""
                        putStrLn $ "\nRunning " ++ solverDir
                        glpkOutput <- readCreateProcess ((shell $ "./" ++ solverDir) {cwd = Just $ solverDir ++ "/src/" }) ""
                        writeFile (solverDir++"/src/"++label++".out") $ glpkOutput
                        -- TODO this prevents glpk from giving a bad schedule when the problem fails
                        -- there should be a more general solution for other solvers
                        let glpkFailed = "LP HAS NO PRIMAL FEASIBLE SOLUTION" `elem` (lines glpkOutput)
                        when glpkFailed $ error "LP HAS NO PRIMAL FEASIBLE SOLUTION"
                        return sc)
               else (return $ error "reuse cannot return HashedData")
          rawOutput <- readFile $ solverDir ++ "/src/data_out.csv"
          rawII <- readFile $ solverDir ++ "/src/" ++ label ++ ".ii"
          putStrLn $ "\n" ++ solverDir ++ " finished, generating modulo codegraph"
          let ii = read rawII :: Double
              prelimSchedule = parseHashedOutput rawOutput
              prelimSchedule' = postProcessPrelimSchedule dfGraph prelimSchedule ii
              -- (cg',sched) = genModuloSchedule2S cg (label,dfGraph) prelimSchedule ii unroll
              (cg',sched) = genModuloScheduleNStages cg (label,dfGraph) prelimSchedule' ii unroll numStages
          -- putStrLn $ "\nprelimSchedule: " ++ show prelimSchedule
          return (cg',sched,ii,stdgen,hashedData)
       Nothing -> error "no modulo body to scheduled"

-- | Can add any post-processing of the preliminary schedule here, including enforcing constraints that cannot
-- be handled by the optimization problem.
-- At the moment, the only thing done here is enforcing that every incMR and modulo load/store pair appear in
-- the same stage. In the future, we should probably add an option to provide ad-hoc rewrites like to
-- genHashedSchedule like we do with penalties / constraints to the optimization problem
postProcessPrelimSchedule :: Hardware h => DataFlowGraph h -> Map Node Double -> Double -> Map Node Double
postProcessPrelimSchedule dfGraph prelimSchedule ii =
  let
    DataFlowGraph fglGraph dfIns dfOuts overwrites incr stage = dfGraph

    getStage n = case Map.lookup n prelimSchedule of
                  Just t -> floor (t / ii) + 1
                  Nothing -> error $ "node missing from prelimSchedule" ++ show n

    -- find all incMR -> moduloVStore/Load pairs
    moduloLoadsStores = filter (\(_,dn) -> isModuloLoad dn || isModuloStore dn) $ labNodes fglGraph
    incMRPairs = concatMap (\(n,dfNode ) ->
                             case filter (isIncMR . snd)  $ instructionProducersL dfGraph n of
                               ((incMRNode,_):_) -> [(incMRNode,n)]
                               _ -> []
                           ) moduloLoadsStores

    -- need to move incMR into the same stage as the modulo load/store that consumes it
    -- need to be care doing this as to not break dependencies, if the modulo load/store
    -- is at the very beginning of the stage we need to push it up but not so far up
    -- that it would be above its consumers, use the fact dependencies are spaced eps apart
    -- NOTE this will introduce a bug if subjectConstraints doesn't use eps
    newSchedules = concatMap updateIncMR incMRPairs
    updateIncMR (incN,mN) = let
        mStage = getStage mN
        time' = fromIntegral (mStage-1) * ii + (eps/2)
        mTime = fromJust $ Map.lookup mN prelimSchedule
      in if mStage == getStage incN
         then []
         else if time' < mTime
                 then [(incN,time')]
                 else [(incN,time')
                      ,(mN,time'+(eps/4))] -- needs to be less than eps to not risk breaking dep
    -- insert new timings into the prelim schedule (overwriting previous timings)
    prelimSchedule' = foldr (\(n,t) m -> Map.insert n t m) prelimSchedule newSchedules
  in prelimSchedule'
     `debug` ("newSchedules: " ++ show newSchedules)

-- | Parses the contents of data_out.csv (i.e., the ipopt solution output file).
-- The file has the following example format:
--   # Generated: Wed Mar 16 22:28:34 2022
--   b3,0,1.0111
--   t3,0,-0.0000
--   ...
parseHashedOutput :: String -> Map Node Double
parseHashedOutput rawOutput =
  let
    csvLines = tail $ lines rawOutput
    varPairs = map (\ln -> case splitOn "," ln of
                       (var:_:res:_) -> (var,read res :: Double)
                       _ -> error $ "failure to parse data_out.csv on line: " ++ ln
                       ) csvLines
    tVarMap = concatMap (\(var,dispatchTime) ->
                           case var of
                             ('t':n) -> [(read n :: Int,dispatchTime)]
                             _ -> []) varPairs
  in Map.fromList tVarMap

-----------------------------------------------------------------------------------------------------------
-- non-hashed
-----------------------------------------------------------------------------------------------------------
-- moduloSchedule2S :: forall h . (Hardware h)
--   => CodeGraph h
--   -> Int
--   -> Maybe (CodeGraph h,Schedule)
-- moduloSchedule2S cg@(CodeGraph cfGraph dfGraphs cgIn cfOut mrTables cTable tags maxID dbgMap) unroll =
--   let
--     -- edgeIsModulo :: LEdge (CFEdge h) -> Bool
--     edgeIsModulo (_,_,CFModuloBody _) = True
--     edgeIsModulo _ = True

--     -- moduloEdges :: [LEdge (CFEdge h)]
--     moduloEdges = filter edgeIsModulo $ labEdges $ controlFlowGraph cfGraph

--     moduloBody = case moduloEdges of
--                    ((n0,n1,CFModuloBody label)):_ -> case List.lookup label dfGraphs of
--                                                        Just dfGraph -> Just (label,dfGraph)
--                                                        Nothing -> error $ "Missing dfGraph: " ++ show label
--                    _ -> Nothing

--     moduloDFGraph = snd $ fromJust $ moduloBody

--     prelimSchedule :: Map Node Double
--     prelimSchedule = Map.fromList $ zip (scheduleDataFlowGraph ("PrelimModulo",moduloDFGraph)) [0..]

--     isInstruction :: Node -> Bool
--     isInstruction n =
--       case match n (dataFlowGraph moduloDFGraph) of
--         (Just (_,_,nodeLbl,_),_) -> case nodeLbl of
--           InstructionNode _ -> True
--           _ -> False
--         (Nothing,_) -> False

--     ii :: Double
--     ii = (fromIntegral $ length $ labNodes $ nfilter isInstruction $ dataFlowGraph moduloDFGraph) / 2

--   in case moduloBody of
--        Just (label,dfGraph) -> Just $ genModuloSchedule2S cg (label,dfGraph) prelimSchedule ii unroll
--        Nothing -> Nothing

-- | Given a @DataFlowGraph@ which is a loop body marked for modulo scheduling, a preliminary schedule
-- and the computed II transform the @CodeGraph@ into a modulo scheduled @CodeGraph@ with a schedule
-- that explicetly covers each remapped node
-- genModuloSchedule2S :: forall h . (Hardware h)
--   => CodeGraph h
--   -> (String, DataFlowGraph h)
--   -> Map Node Double
--   -> Double
--   -> Int
--   -> (CodeGraph h,Schedule)
-- genModuloSchedule2S cg (label,dfGraph) prelimSchedule ii unroll =
--   let
--     DataFlowGraph fglGraph dfIn dfOut overwrites incr stage = dfGraph

--     lessThanII :: Node -> Bool
--     lessThanII n = let
--         dTime = case Map.lookup n prelimSchedule of
--                   Just t -> t
--                   Nothing -> error $ "bad lookup: " ++ show n ++ "\n in schedule: " ++ show prelimSchedule
--       in dTime < ii

--     dfGraph0,dfGraph1 :: DataFlowGraph h
--     (dfGraph0,dfGraph1) = partGraphByInstr (label,dfGraph) lessThanII
--     dfGraph0' =
--       let
--         dfInputs' = filter (\(tag,_) -> not $ ':' `elem` tag) $ dataFlowInputs dfGraph0
--                     -- `debug` ("\ndfGraph0 outputs: " ++ ( show $ dataFlowOutputs dfGraph0 ) ++ "\n")
--       in dfGraph0 { dataFlowInputs = dfInputs' }
--     dfGraph1' =
--       let
--         dfOutputs' = filter (\(tag,_) -> not $ ':' `elem` tag) $ dataFlowOutputs dfGraph1
--                     -- `debug` ("\ndfGraph1 inputs: " ++ ( show $ dataFlowInputs dfGraph1 ) ++ "\n")
--       in dfGraph1 { dataFlowOutputs = dfOutputs' }

--     inMRNode = case concatMap (\(label,n) -> if nodeTagIsInMR label
--                                                 then [n]
--                                                 else [])  (dataFlowInputs dfGraph) of
--                  (n:_) -> n
--                  [] -> error "genModuloSchedule2S dataflowgraph contains no mrIn input"
--     allInMRNodes = groupMROverwrites $ IntSet.fromList [inMRNode]
--     groupMROverwrites set = let
--         set' = foldr (IntSet.insert) set $ concatMap (\(n0,n1) ->
--                                                         if n0 `IntSet.member` set || n1 `IntSet.member` set
--                                                         then [n0,n1]
--                                                         else []) $ map snd overwrites
--       in if set == set' then set else groupMROverwrites set'

--     -- merge dfGraph0 and dfGraph1 to run in parallel
--     -- dfGraphS0S1 = DataFlowGraph (gmap incrementS0Loads fglGraph) dfIn dfOut
--     dfGraphS0S1 = DataFlowGraph s0s1GraphIncremented s0s1DFIn s0s1DFOut overwrites incr 0
--       where
--         incr = dataFlowIncrement dfGraph
--         maxNode = snd $ nodeRange fglGraph
--         s0Tags = dataFlowOutputs dfGraph0
--         s1Tags = dataFlowInputs dfGraph1
--         -- registers that are shared between s0/s1 (not including inMR/outMR)
--         s0SharedRegs = map snd $ filter hasS0Tag s0Tags
--         s1SharedRegs = map snd $ filter hasS1Tag s1Tags
--         hasS0Tag (('s':'0':':':_),_) = True `debug` "hasS0Tag triggered"
--         hasS0Tag _ = False
--         hasS1Tag (('s':'1':':':_),_) = True `debug` "hasS1Tag triggered"
--         hasS1Tag _ = False
--         (s0Nodes,s0Edges) = (labNodes $ dataFlowGraph dfGraph0,labEdges $ dataFlowGraph dfGraph0)
--         (s1Nodes,s1Edges) = (labNodes $ dataFlowGraph dfGraph1,labEdges $ dataFlowGraph dfGraph1)
--         -- remap s1 shared regs so they can serve as inputs (not this shouldnt include inMR/outMR)
--         s1Nodes' = map (\(n,nL) -> if n `elem` s1SharedRegs then (n+maxNode,nL) else (n,nL)) s1Nodes
--         s1Edges' = map (\(n0,n1,eL) -> case (n0`elem` s1SharedRegs,n1 `elem` s1SharedRegs) of
--                            (True,True) -> (n0+maxNode,n1+maxNode,eL)
--                            (False,True) -> (n0,n1+maxNode,eL)
--                            (True,False) -> (n0+maxNode,n1,eL)
--                            (False,False) -> (n0,n1,eL)
--                        ) s1Edges
--         -- remove edges from s0 output regs to s1 instructions
--         s0Edges' = filter (\(n0,n1,eL) -> not $ n0 `elem` s0SharedRegs) s0Edges
--         s0s1Nodes = s1Nodes' ++ s0Nodes
--         s0s1Edges = s1Edges' ++ s0Edges'
--         s0s1Graph = (mkGraph s0s1Nodes s0s1Edges)
--         s0s1GraphIncremented = gmap incrementS0Loads s0s1Graph
--         -- FIXME is all the s0SharedRegs/s1SharedRegs stuff unnecessary, and the inputs are set
--         -- by taking s1Tags `union` dfIn? are the input nodes to s1 not unique from the outputs of s0?
--         s0s1DFIn = s1Tags `unionByTag` dfIn
--         s0s1DFOut = s0Tags `unionByTag` dfOut

--     incrementS0Loads (prevs,node,dfNode,succs)
--       | node `elem` s0Nodes && ((IntSet.toList allInMRNodes ) `List.union` (map snd prevs) /= [])
--          = (prevs,node,dfNode',succs)
--       | otherwise = (prevs,node,dfNode,succs)
--       where
--         s0Nodes = map fst $ labNodes $ dataFlowGraph dfGraph0
--         dfNode' = case dfNode of
--                     InstructionNode (Instruction imm name fn) -> case name of
--                       -- FIXME support all instructions that use mrIn?
--                       "moduloVLoad" -> InstructionNode $ Instruction (map (+(16*unroll)) imm) name
--                                   (\m0 m ->
--                                       moduloVLoad @Interp m0 m (fromIntegral (head imm) + (fromIntegral $ (16*unroll))))
--                                  `debug` ("moduloVLoad incremented: "++show node)
--                       _ -> dfNode
--                     _ -> dfNode

--     nodeTagIsMR ('m':'r':_) = True
--     nodeTagIsMR _ = False

--     nodeTagIsInMR ('m':'r':'I':'n':_) = True
--     nodeTagIsInMR _ = False

--     initCGBuilder = codeGraphToCGBuilder cg
--     cg' = createCGFromCGB initCGBuilder -- buildModuloCG'
--                         $ buildModuloCG label 2 unroll cg [dfGraph0'] [dfGraph1'] dfGraphS0S1

--     dfGraphs' = cgDataFlows cg'

--     allInitMRs = concatMap (\(_,dfGraph) -> map (\(n,dfNode) ->
--                                                      case dfNode of
--                                                        (InstructionNode (InitMR name _)) -> (n,name)
--                                                        _ -> error $ "encountered non-InitMR node")
--                               $ initMRNodes dfGraph) dfGraphs'
--     newMRTables = map (\(name,node,vrs) ->
--                          case filter (\(n,nm) -> name==nm) allInitMRs of
--                            [] -> (name,node,vrs)
--                            ns -> (name,minimum $ map fst ns,vrs)
--                           ) $ cgMRTables cg
--     cgWithMRs = cg' { cgMRTables = newMRTables }

--     -- map all dataflowgraphs in dfGraphs' to a schedule
--     dfGraphToSchedule (label',dfGraph')
--       | endingTag `elem` ["S0S1","S0","S1"] =
--           -- these graphs are the result of a cgbInsertDataFlow and each node
--           -- corresponds to a node in the original schedule (this node is (n-incr))
--              Map.fromList [ (n,let t = fromJust $ Map.lookup (n-incr) prelimSchedule
--                                 in if t > ii then t-ii else t) | n <- nodes]
--       | otherwise = Map.fromList
--                     $ zip (scheduleDataFlowGraph ("In dfGraphToSchedule: "++label',dfGraph' )) [0..]
--       where
--         -- the part of the label proceeding the final _
--         endingTag = case splitOn "_" label' of
--                       [] -> ""
--                       ss -> last ss
--         -- all instruction nodes
--         nodes = map fst
--                 $ filter (\(_,dfNode) -> isInstruction dfNode && not (isInitMR dfNode))
--                 $ labNodes $ dataFlowGraph dfGraph'
--         -- if the dfGraph is the result of a cgbInsertDataFlow, incr represents
--         -- the incrementation used when remapping the node id's from the original graph
--         incr = dataFlowIncrement dfGraph'

--     schedule' = foldr Map.union Map.empty $ map dfGraphToSchedule dfGraphs'
--   in (cgWithMRs,schedule')

-- | Take a @CodeGraph@ with a @CFModuloBody@ edge and generate a new
-- @CodeGraph@ that runs that edge in a modulo scheduled loop
genModuloScheduleNStages cg (label,dfGraph) prelimSchedule ii unroll numStages =
  let
    DataFlowGraph fglGraph dfIn dfOut overwrites incr stage = dfGraph

    withinStage :: Int -> Node -> Bool
    withinStage stage n =
      let
        dTime = case Map.lookup n prelimSchedule of
                  Just t -> t
                  Nothing -> error $ "bad lookup: " ++ show n ++ "\n in schedule: " ++ show prelimSchedule
      in dTime < (fromIntegral stage + 1) * ii
      -- in (fromIntegral stage ) * ii <= dTime
      --    && dTime < (fromIntegral stage + 1) * ii

    inOutMRNodes :: IntSet
    inOutMRNodes = findAllInOutMRNodes dfGraph

    -- fifoPtrNodes :: (Node,Node)
    -- fifoPtrNodes = case filter (\(label,n) ->
    --                           case label of
    --                             ('f':'i':'f':'o':_) -> True
    --                             _ -> False)  (dataFlowInputs dfGraph) of
    --              ([("fifo0",n0),("fifo1",n1)]) -> (n0,n1)
    --              ([("fifo1",n1),("fifo0",n0)]) -> (n0,n1)
    --              _ -> error "dataflowgraph contains no fifo0/fifo1 input"

    unionTags :: [String]
    unionTags = ["cnt","mrIn","mrOut","size"]

    -- Partitioned DataFlowGraphs
    -- Stages | DFGraphs
    -- 1      | [S0]
    -- 2      | [S0,S1]
    -- 3      | [S0,S1,S2]
    -- ...
    partDFGraphs0 = partNGraphByInstr (label,dfGraph) withinStage numStages
    partDFGraphs =  --map (\(s,g) -> addFifoSpills s fifoPtrNodes g) $ zip [0..]
        partDFGraphs0
    mSched = modulizePrelimSchedule prelimSchedule ii partDFGraphs

    -- Build Prologue
    -- Stages  |  PrologueParts
    -- 1       | []
    -- 2       | [(0,[S0])]
    -- 3       | [(0,[S0,S1]),(1,[S0])]
    -- ...
    prologueParts =
      map (\(incr,dfGraphs) ->
             map (\g -> incrementLoadsStores unionTags inOutMRNodes incr unroll g) dfGraphs)
      $ zip [0..] (reverse [ take i partDFGraphs | i <- [1..(numStages-1) ] ] )
      -- `debug` ("\npartDFGraphs:\n"++show partDFGraphs++"\n")
    prologueDFGraphs = map (removeTaggedInputs . unPartDFGraphs) prologueParts
    -- (prologueDFGraph0,prologueSched) =
    --   parallelMergeDFGraphs (map unPartDFGraphs prologueParts) mSched unionTags
    -- TODO only remove tags from first prologue
    removeTaggedInputs dfGraph =
      let
        dfInputs' = filter (\(tag,_) -> not $ ':' `elem` tag) $ dataFlowInputs dfGraph
      in dfGraph { dataFlowInputs = dfInputs' }

    -- Build Epilogue
    -- Stages  |  EpilogueParts
    -- 1       | []
    -- 2       | [(0,[S1])]
    -- 3       | [(-1,[S2]),(0,[S1,S2])]
    -- ...
    epilogueParts =
      reverse
      $ map (\(incr,dfGraphs) ->
               map (\g -> incrementLoadsStores unionTags inOutMRNodes incr unroll g) dfGraphs)
      $ zip (reverse [-(numStages-1)..0]) [ drop i partDFGraphs | i <- [1..(numStages-1)] ]
    epilogueDFGraphs = map (removeTaggedOutputs . unPartDFGraphs) epilogueParts
    -- (epilogueDFGraph0,epilogueSched) =
    --   parallelMergeDFGraphs (map unPartDFGraphs epilogueParts) mSched unionTags
    -- TODO only remove tags from last epilogue
    removeTaggedOutputs dfGraph =
      let
        dfOutputs' = filter (\(tag,_) -> not $ ':' `elem` tag) $ dataFlowOutputs dfGraph
      in dfGraph { dataFlowOutputs = dfOutputs' }

    -- Build Kernel
    -- Stages  |  KernelParts
    -- 1       | [(0,S0)]
    -- 2       | [(1,S0),(0,S1)]
    -- 3       | [(2,S0),(1,S1),(0,S2)]
    -- ...
    kernelParts = map (\(incr,g) -> incrementLoadsStores unionTags inOutMRNodes incr unroll g)
      $ reverse $ zip [0..] $ reverse partDFGraphs
    (kernelDFGraph,kernelSched) = parallelMergeDFGraphs kernelParts mSched unionTags

    -- Build Tail (unmodulorized body of the loop that loads/stores from scratch space)
    tailDFGraph = dfGraph { dataFlowInputs = tailInputs
                          , dataFlowOutputs = tailOutputs
                          -- ,dataFlowGraph = tailGraph
                          } `debug` ("tailInputs: " ++ show tailInputs)
    swapMRLabels (lbl,n) = case lbl of
                            "mrIn" -> ("scratchIn",n)
                            "mrOut" -> ("scratchOut",n)
                            "cnt" -> ("fakecnt",n)
                            _ -> (lbl,n)
    -- TODO filtering out fifo's (because of lack of GPRs), will cause a bug if there is any actual fifo spills
    removeFifos (lbl,_) = case lbl of
                            'f':'i':'f':'o':_ -> False
                            _ -> True
    tailInputs = filter removeFifos $ map swapMRLabels $ dataFlowInputs dfGraph
    tailOutputs = filter removeFifos $ map swapMRLabels $ dataFlowOutputs dfGraph
    -- tailGraph = nmap swapLoadsStores fglGraph
    -- TODO need to adjust this to support scalar code
    -- swapLoadsStores node
    --   | isModuloStore node =
    --       case node of
    --         InstructionNode (Instruction imms _ _) -> InstructionNode (ScratchStore imms "scratchStoreVR")
    --         _ -> error "bad pattern match on moduloStore"
    --   | isModuloLoad node =
    --       case node of
    --         InstructionNode (Instruction imms _ _) -> InstructionNode (ScratchStore imms "scratchLoadVR")
    --         _ -> error "bad pattern match on moduloLoad"

    -- create new codegraph
    initCGBuilder = codeGraphToCGBuilder cg
    cg' = cgApplyRewrites
          $ createCGFromCGB initCGBuilder
          $ buildModuloCG label numStages unroll cg prologueDFGraphs epilogueDFGraphs kernelDFGraph tailDFGraph

    dfGraphs' = cgDataFlows cg'

    -- update mrTable
    allInitMRs = concatMap (\(_,dfGraph) -> map (\(n,dfNode) ->
                                                     case dfNode of
                                                       (InstructionNode (InitMR name _)) -> (n,name)
                                                       _ -> error $ "encountered non-InitMR node")
                              $ initMRNodes dfGraph) dfGraphs'
    newMRTables = map (\(name,node,vrs) ->
                         case filter (\(n,nm) -> name==nm) allInitMRs of
                           [] -> (name,node,vrs)
                           ns -> (name,minimum $ map fst ns,vrs)
                          ) $ cgMRTables cg'
    cgWithMRs = cg' { cgMRTables = newMRTables }

    -- combine all modulo scheduled code into one schedule
    mSched' =  mSched `Map.union` kernelSched  -- `Map.union` prologueSched `Map.union` epilogueSched
    schedule' = foldr Map.union Map.empty $ map (dfGraphToSchedule mSched' prelimSchedule) dfGraphs'
  in (cgWithMRs,schedule')
     -- `debug`
     --  ("\n\nInitMRs before: " ++ show partDFGraphs
     --   ++ "\n\nNew InitMRs: " ++ show mSched
     --   ++ "\n")

-- generate schedule for new codegraph
dfGraphToSchedule sched prelimSched (label',dfGraph')
  | endingTag `elemContains` ["PROLOGUE","EPILOGUE","KERNEL"] =
      -- these graphs are the result of a cgbInsertDataFlow and each node
      -- corresponds to a node in the original schedule (this node is (n-incr))
          Map.fromList [ (n,case Map.lookup (n-incr) sched of
                            Just t -> t
                            Nothing -> error $ "dfGraphToSchedule missing node: " ++ show (n-incr)
                        ) | n <- nodes]

  | endingTag == "TAIL" =
          Map.fromList [ (n,case Map.lookup (n-incr) prelimSched of
                            Just t -> t
                            Nothing -> error $ "Tail missing node: " ++ show (n-incr)
                        ) | n <- nodes]
          `debug` ("Processed dfGraphToSchedule TAIL")
  | otherwise = Map.fromList
                $ zip (scheduleDataFlowGraph ("In dfGraphToSchedule: "++label',dfGraph' )) [0..]
  where
    -- the part of the label proceeding the final _
    endingTag = case splitOn "_" label' of
                  [] -> ""
                  ss -> last ss
    startsWith (l0:lbl0) (l1:lbl1) = l0 == l1 && startsWith lbl0 lbl1
    startsWith _ [] = True
    startsWith [] _ = False

    elemContains lbl0 = any (startsWith lbl0)
    -- all instruction nodes
    nodes = map fst
            $ filter (\(_,dfNode) -> isInstruction dfNode && not (isInitMR dfNode))
            $ labNodes $ dataFlowGraph dfGraph'
    -- if the dfGraph is the result of a cgbInsertDataFlow, incr represents
    -- the incrementation used when remapping the node id's from the original graph
    incr = dataFlowIncrement dfGraph'

-- Take a list of DataFlowGraph's (created with partNGraphByInstr)
modulizePrelimSchedule :: Schedule -> Double -> [DataFlowGraph h] -> Schedule
modulizePrelimSchedule sched ii dfGraphs =
  let
    modulizeDFGraph dfGraph =
      let
        incr = dataFlowIncrement dfGraph
        stage = dataFlowStage dfGraph
        instrNodes = map fst
                     $ filter (\(_,dfNode) -> isInstruction dfNode && not (isInitMR dfNode))
                     $ labNodes $ dataFlowGraph dfGraph
      in Map.fromList [ (n,case Map.lookup (n-incr) sched of
                            Just t ->  t - (fromIntegral  stage)*ii
                            Nothing -> error $ "modulizePrelimSchedule missing node " ++ show n
                        ) | n <- instrNodes ]
  in foldr Map.union sched $ map modulizeDFGraph dfGraphs

-- | Adds pointers to FIFOS data section to spills
--
-- When reconstructing the data flow graph for modulo scheduling, there's an
-- opportunity to increase the possible schedule space by spilling nodes for
-- longer than an II, but this will cause the next stages spill to happen before
-- the previous stages despill. To prevent overwriting, we have two seperate
-- ptrs to two different spill spaces, different stages access different spill
-- spaces. NOTE this still constraints spills/despills to happen in less than 2*II
--
-- Standard spills/despills default to using just a label, i.e. str r0,LABEL+off
-- So they take only one argument. When constructing the modulo codegraph we
-- need to add the extra pointers from the label, so str r0,LABEL+off(ptr).
-- That's what this function does by simply adding the appropriate edges to the
-- appropriate pointer based off the stage
addFifoSpills :: Int -> (Node,Node) -> DataFlowGraph h -> DataFlowGraph h
addFifoSpills stage (fifoPtr0,fifoPtr1) dfGraph =
  let
    -- select which fifo space to spill to based on stage
    fifoPtr = case stage `mod` 2 of
                0 -> fifoPtr0
                _ -> fifoPtr1

    fglGraph = dataFlowGraph dfGraph
    spillNodes = nodes $ labfilter nodeIsSpill fglGraph
    nodeIsSpill dfNode = case dfNode of
                           (InstructionNode (Spill _)) -> True
                           (InstructionNode (Despill _)) -> True
                           _ -> False

    newEdges = map (\n -> (fifoPtr,n,DFEdge 1)) spillNodes
    fglGraph' = insEdges newEdges fglGraph

  in dfGraph { dataFlowGraph = fglGraph'
             , dataFlowInputs =  dataFlowInputs dfGraph
             , dataFlowOutputs = dataFlowOutputs dfGraph }

incrementLoadsStores :: Hardware h => [String] -> IntSet -> Int -> Int -> DataFlowGraph h -> DataFlowGraph h
incrementLoadsStores unionTags loadStoreSet incr scale dfGraph =
  let
    incrementLoadsStores' (prevs,node,dfNode,succs)
      | nodeIsInLoadStores = (prevs,node,dfNode',succs)
      | otherwise = (prevs,node,dfNode,succs)
      where
        nodeIsInLoadStores = IntSet.toList loadStoreSet `List.union` map snd prevs /= []
        dfNode' =
          case dfNode of
            -- TODO this needs to be handled by Hardware instance
            instr@(InstructionNode (Instruction imm name fn)) ->
              case name of
               "moduloVLoad" -> InstructionNode $ Instruction (map (+(16*incr*scale)) imm) name
                (\m0 m ->
                  moduloVLoad @Interp m0 m (fromIntegral (head imm) + fromIntegral (16*incr*scale)))
               "moduloVStore" -> InstructionNode $ Instruction (map (+(16*incr*scale)) imm) name
                (\m0 m v0 ->
                  moduloVStore @Interp m0 m (fromIntegral (head imm) + fromIntegral (16*incr*scale)) v0)
               "moduloVLoadG" -> InstructionNode $ Instruction (map (+(16*incr*scale)) imm) name
                (\m0 m ->
                  moduloVLoadG @Interp m0 (fromIntegral (head imm) + fromIntegral (16*incr*scale)) m)
               "moduloVStoreG" -> InstructionNode $ Instruction (map (+(16*incr*scale)) imm) name
                (\m0 m v0 ->
                  moduloVStoreG @Interp m0 (fromIntegral (head imm) + fromIntegral (16*incr*scale)) m v0)
               _ -> dfNode
            _ -> dfNode

  in dfGraph { dataFlowGraph = gmap incrementLoadsStores' $ dataFlowGraph dfGraph
             , dataFlowInputs =  dataFlowInputs dfGraph
             , dataFlowOutputs = dataFlowOutputs dfGraph }

{- |
   |                                        |----------------------------------------------------|                                                     |------------------|                                                  |-----------------|
                                            v                                                    |                                                     v                  |                                                  v                 |
 (i)--init---(i)---(p)--prologue[0]--(p)--(lh)--loophead--(lh)--(k)--Kernel--(k)--(le)-loopend--(le) (e)---epilogue---(e)---(si)---storeinit---(si)---(ts)---tailstore---(ts)---(t)---tail---(t)---(li)---loadinit---(li)---(tl)--(tailload)---(tl)--(r)
              |                                            |                                          ^                      ^                   |                                                                                                    ^
              |                                            -------------------------------------------|                      |                   -----------------------------------------------------------------------------------------------------|
              |                                                                                                              |
              |---------------------------------------------------------------------------------------------------------------
 -}
buildModuloCG :: forall h . Hardware h
              => String
              -> Int
              -> Int
              -> CodeGraph h
              -> [DataFlowGraph h]
              -> [DataFlowGraph h]
              -> DataFlowGraph h
              -> DataFlowGraph h
              -> CGBState h ()
buildModuloCG label numStages unroll cg dfGraphsP dfGraphsE dfGraphK tailDFGraph =
  let
    prologueLbl n = map toUpper label ++ "_PROLOGUE" ++ show n
    kernelLbl = map toUpper label ++ "_KERNEL"
    epilogueLbl n = map toUpper label ++ "_EPILOGUE" ++ show n
    loopHeadLbl = map toUpper label ++ "_LOOPHEAD"
    tailLbl = map toUpper label ++ "_TAIL"
    -- first prologue and rest
    (dfGraphP0,dfGraphP') =
      case dfGraphsP of
        (dfGraphP0:dfGraphP') -> (dfGraphP0,dfGraphP')
          -- `debug` ("\ndfGraphsP: \n"++show dfGraphsP++"\n")
        [] -> error "buildModuloCG given no prologue"
    -- final epilogue and rest
    (dfGraphE0,dfGraphE') =
      case reverse dfGraphsE of
        (dfGraphE0:dfGraphE') -> (dfGraphE0,dfGraphE')
          -- `debug` ("\ndfGraphsE: \n"++show dfGraphsE++"\n")
        [] -> error "buildModuloCG given no epilogue"

  in do initB <- genBranchBlock $ moduloInitBlock @h (label,unroll)
        prologueB0 <- genBlock $ dfGraphToBlock (prologueLbl 0) dfGraphP0
        epilogueB0 <- genBlock $ dfGraphToBlock (epilogueLbl 0) dfGraphE0
        (prologueBN,epilogueBN) <- genPrologueEpilogue label (prologueB0,epilogueB0) dfGraphE' dfGraphP' []
        -- -- (prologue) --COMPOSE-- (loophead)
        loopHeadB <- genBranchBlock $ moduloLoopHeadBlock @h numStages (label,unroll)
        composeBP (==) prologueBN loopHeadB
        --            /-EQ-(epilogue)
        -- (loophead)-
        --            \-NE-(kernel)
        kernelB <- genBlock $ dfGraphToBlock kernelLbl dfGraphK
        branchBP cmpTagsByStage loopHeadB epilogueBN kernelB
        -- (kernel) --COMPOSE-- (loopend) --COMPOSE-- (loophead)
        loopEndB <- genBranchBlock $ blockAddJump $ moduloLoopEndBlock @h (label,unroll)
        composeBP (==) kernelB loopEndB
        jumpBP (==) loopEndB loopHeadB
        -- remove fifo selectors from epilogueB0 so as to not project them forward into the tail (free up GPRs)
        -- let epilogueOuts = bpDFOutputs epilogueB0
        --     removeFifos (lbl,_) = case lbl of
        --                             'f':'i':'f':'o':_ -> False
        --                             _ -> True
        --     epilogueOuts' = filter removeFifos epilogueOuts
        --     eCFNode' = CFNode epilogueOuts'
        -- cgbUpdateCFNode (bpCFOutput epilogueB0) eCFNode'
        -- (epilogue) --COMPOSE--(storeinit)
        postincr <- genBlock $ postCntIncrBlock @h (label,unroll)
        storeinitB <- genBranchBlock $ tailStoreInitBlock @h (label,unroll)
        --        /-NE-(prologue)
        -- (init)-
        --        \-EQ-(return)
        branchBP (==) initB storeinitB prologueB0
        -- composeBP (==) (epilogueB0 { bpDFOutputs = epilogueOuts' }) postincr storeinitB
        composeBP (==) epilogueB0 postincr
        composeBP (==) postincr storeinitB
        --
        tailstoreB <- genBranchBlock $ tailStoreLoopHeadBlock @h (label,unroll)
        tailB <- genBlock $ dfGraphToBlock tailLbl tailDFGraph
        loadinitB <- genBlock $ tailLoadInitBlock @h (label,unroll)
        tailloadB <- genBranchBlock $ tailLoadLoopHeadBlock @h (label,unroll)
        returnNode <- cgbAddCFNode $ CFNode []
        let returnB = BlockParams { bpLabel = "RETURN"
                                  , bpCFInput = returnNode
                                  , bpCFOutput = returnNode
                                  , bpDFInputs = []
                                  , bpDFOutputs = []
                                  }
        --  v-------------|                     v-----------|
        -- -- (tailstore)---(tail)--(loadinit)---(tailload)--
        branchBP (==) tailstoreB tailstoreB tailB
        composeBP (==) tailB loadinitB
        composeBP (==) loadinitB tailloadB
        branchBP (==) tailloadB tailloadB returnB
        --             /-NE-(tailstore)
        -- (storeinit)-
        --             \-EQ-(return)
        branchBP (==) storeinitB returnB tailstoreB
        -- -- substitue (m0) --CFModuloBody-- (m1) for new ControlFlowGraph
        cgbSubCFNode (cgInput cg) (bbpCFInput initB)
        cgbSubCFNode (cgOutput cg) (bpCFOutput returnB)
        cgbRemoveCFEdge (bbpCFInput initB,bpCFOutput returnB) -- Remove CFModuloBody edge
        cgbUpdateCFNode (bpCFOutput returnB) $ CFNode []
        cgbSetCFInsOuts (bbpCFInput initB,bpCFOutput returnB)
        cgbRemoveDFGraph label

-- | When numStages > 2, multiple prologues and epilogues need to be composed
-- together. If the inputs size is too small, we need to branch before
-- a prologue is executed to the appropriate epilogue
genPrologueEpilogue :: forall h . Hardware h
                    => String
                    -> (BlockParams,BlockParams)
                    -> [DataFlowGraph h]
                    -> [DataFlowGraph h]
                    -> [BlockParams]
                    -> CGBState h (BlockParams,BlockParams)
genPrologueEpilogue label (pN,e0) []     []     (eN:eBPs) = return (pN,e0)
genPrologueEpilogue label (pN,e0) []     []     [] = return (pN,e0)
genPrologueEpilogue label (p0,e0) (e:es) (p:ps) eBPs =
 let
   n = length eBPs + 1
   prologueLbl n = map toUpper label ++ "_PROLOGUE" ++ show n
   epilogueLbl n = map toUpper label ++ "_EPILOGUE" ++ show n
 in do e1 <- genBlock $ dfGraphToBlock (epilogueLbl n) e
       p1 <- genBlock $ dfGraphToBlock (prologueLbl n) p
       -- NOTE use back projected compose to construct epilogue backwords
       composeBP_B (==) e1 e0
       skipB <- genBranchBlock $ moduloSkipBlock @h label n
       composeBP (==) p0 skipB
       branchBP (==) skipB e0 p1
       genPrologueEpilogue label (p1,e1) es ps (e0:eBPs)
genPrologueEpilogue _ _ es ps _ = error "mismatched amount of prologues/epilogues"

-- | Simple branch designed to skip prologue sections if the input size isn't
-- large enough
moduloSkipBlock :: forall h . (Hardware h, CoreISA (Graph h))
  => String -> Int -> BranchBlock h ((MR,MR,GPR,GPR),(MR,MR,GPR,GPR))
moduloSkipBlock label prologueNum =
  let
    moduloSkip :: forall repr . CoreISA repr
               => (repr MR,repr MR,repr GPR,repr GPR)
               -> (repr BRANCH,(repr MR, repr MR,repr GPR,repr GPR))
    moduloSkip (mrIn,mrOut,cnt,size) =
      let
        branchRes = branchImmNotHigh size prologueNum
      in (branchRes,(mrIn,mrOut,cnt,size))
  in branchingBlock ((map toUpper label)++"_SKIP"++show prologueNum)
                    ["mrIn","mrOut","cnt","size"]
                    ["brResI","mrIn","mrOut","cnt","size"]
                    (moduloSkip @(Graph h))

-- TODO dynamically adjust this based on actual number of fifos?
fifoSpaceSize = 16 * numBytesInVR
numBytesInVR = 16 -- TODO is this the correct scaling

-- | Initializes a counter and jumps if gSiz is equal to zero
-- ```
-- count = 0
-- if size == 0
--   jump RETURN
-- ```
moduloInitBlock :: forall h . (Hardware h, CoreISA (Graph h))
  => (String,Int) -> BranchBlock h ((MR,MR,GPR),(MR,MR,GPR,GPR))
moduloInitBlock (label,unroll) =
  let
    moduloInit :: forall repr . CoreISA repr
               => (repr MR,repr MR,repr GPR)
               -> (repr BRANCH,(repr MR, repr MR,repr GPR,repr GPR))
    moduloInit (mrIn,mrOut,size) =
          let
            cnt = xorG size size -- unintegerG 0
            -- fifo0 = xorG cnt cnt -- unintegerG 0
            -- fifo1 = unintegerG $ fromIntegral spillSpaceSize * 8 -- 8 bytes per Word64
            -- zero = unintegerG 0
            -- c0 = cmpGPRs size zero
            -- branchRes = je c0
            branchRes = branchImmNotHigh size (16*unroll-1)
          in (branchRes,(mrIn,mrOut,cnt,size))
  in branchingBlock ((map toUpper label)++"_INIT")
     ["mrIn","mrOut","size"]
                    ["brResI","mrIn","mrOut","cnt","size"]
                    (moduloInit @(Graph h))

-- TODO moduloLoadScratchBlock
 -- need to compute size of scratch
-- NLT8 DS 0H

--     agfi r5,128 * hardcoded for 2 stages
--     CIJL   r1,1,PRERETURN   * r1 < 1
--     lgr     r8,r1
--     agfi    r8,-8
--     sgr     r15,r15


-- needs to load vector into
-- LOADSCRATCH DS 0H
--     ld      0,0(R2,r8)
--     std     0,192(r4,r8)   * 192 is VRSAVE+FRSAVE
--     agfi    r8,-8
--     CGIJNL  r8,0,LOADSCRATCH

-- TODO storeScratchBlock

-- | Header code for a while loop, jumps if the counter is equal to size - 1
-- (NOTE extra arguments carried through loop iterations need to be manually
-- tacked on to the input/output CFNode's constructed from this block)
-- ```
-- if cnt == size - (numStages - 1)
--   jump S1_POST
-- ```
moduloLoopHeadBlock :: forall h . (Hardware h,CoreISA (Graph h))
  => Int -> (String,Int) -> BranchBlock h ((MR,MR,GPR,GPR),(MR,MR,GPR,GPR))
moduloLoopHeadBlock numStages (label,unroll) =
  let
    -- numStages = 2 -- TODO pass along numStages
    loopHead :: forall repr . CoreISA repr
             => (repr MR,repr MR,repr GPR,repr GPR)
             -> (repr BRANCH,(repr MR, repr MR,repr GPR,repr GPR))
    loopHead (mrIn,mrOut,cnt,size) =
      let
        stageMinus1 = unintegerG (fromIntegral $ 16*unroll*(numStages-1) + 16*unroll -1)
        -- stageMinus1 = unintegerG (fromIntegral $ 16*unroll*(numStages-1))
        range = subG size stageMinus1 -- (16 * (numStages-1)) -- subtract number of bytes per vector from size
        -- fifoSize = unintegerG $ fromIntegral spillSpaceSize * 8
        -- fifo0' = xorG fifo0 fifoSize
        -- fifo1' = xorG fifo1 fifoSize
        branchRes = branchNotLow cnt range
      in (branchRes,(mrIn,mrOut,cnt,size))
  in branchingBlock ((map toUpper label)++"_LOOPHEAD")
                    ["mrIn","mrOut","cnt","size"]
                    ["brResLH","mrIn","mrOut","cnt","size"]
                    (loopHead @(Graph h))
    `debug` ("stageMinus1: " ++ show (fromIntegral $ 16*unroll*(numStages-1) + 16*unroll -1))

-- | End of a while loop (simply increments a counter)
-- (NOTE extra arguments carried through loop iterations need to be manually
-- tacked on to the input/output CFNode's constructed from this block)
-- ```
-- cnt = cnt + 16 * unroll
-- jump LOOPHEAD
-- ```
moduloLoopEndBlock :: forall h . (Hardware h,CoreISA (Graph h))
  => (String,Int) -> Block h ((MR,MR,GPR,GPR),(MR,MR,GPR,GPR))
moduloLoopEndBlock (label,unroll) =
  let
    loopEnd :: forall repr . CoreISA repr
            => (repr MR,repr MR,repr GPR,repr GPR)
            -> (repr MR,repr MR,repr GPR,repr GPR)
    loopEnd (mrIn,mrOut,cnt,size) =
      let
        incr = unintegerG (16 * fromIntegral unroll)
        cnt' = addG cnt incr
      in (mrIn,mrOut,cnt',size)
  in basicBlock ((map toUpper label)++"_LOOPEND")
                 ["mrIn","mrOut","cnt","size"]
                 ["mrIn","mrOut","cnt","size"]
                 (loopEnd @(Graph h))

postCntIncrBlock :: forall h . (Hardware h,CoreISA (Graph h))
  => (String,Int) -> Block h ((MR,MR,GPR,GPR)
                             ,(MR,MR,GPR,GPR))
postCntIncrBlock (label,unroll) =
  let
    postCntIncr :: forall repr . CoreISA repr
            => (repr MR,repr MR,repr GPR,repr GPR)
            -> (repr MR,repr MR,repr GPR,repr GPR)
    postCntIncr (mrIn,mrOut,cnt,size) =
      let
        incr = unintegerG (16 * fromIntegral unroll)
        cnt' = addG cnt incr
      in (mrIn,mrOut,cnt',size)
  in basicBlock (map toUpper label++"_POSTCNTINCR")
                 ["mrIn","mrOut","cnt","size"]
                 ["mrIn","mrOut","cnt","size"]
                 (postCntIncr @(Graph h))

tailStoreInitBlock :: forall h . (Hardware h,CoreISA (Graph h))
  => (String,Int) -> BranchBlock h ((MR,MR,GPR,GPR)
                                   ,(MR,MR,MR,MR,GPR,GPR,GPR,GPR))
tailStoreInitBlock (label,unroll) =
  let
    tailStoreInit :: forall repr . CoreISA repr
            => (repr MR,repr MR,repr GPR,repr GPR)
            -> (repr BRANCH,(repr MR,repr MR,repr MR, repr MR,repr GPR,repr GPR,repr GPR,repr GPR))
    tailStoreInit (mrIn,mrOut,cnt,size) =
      let
        scratch = initMR (fromIntegral unroll) "SCRATCH" $ replicate unroll $ InterpVR (0,0)
        iter = xorG size size -- same as loading zero
        fakecnt = xorG cnt cnt -- needs to be a distinct instruction form iter
                               -- TODO COCONUT isn't able to copy registers when needed
                               -- also if you attempt to use unintegerG 0 here it causes a bug
        -- -- need to increment counter beyond whats completed in by epilogue
        -- incr = unintegerG (16 * fromIntegral unroll)
        -- cnt' = addG cnt incr
        branchRes = branchNotLow cnt size
      in (branchRes,(mrIn,mrOut,scratch,scratch,iter,fakecnt,cnt,size))
  in branchingBlock (map toUpper label++"_TAILSTOREINIT")
                 ["mrIn","mrOut","cnt","size"]
                 ["brResTI","mrIn","mrOut","scratchIn","scratchOut","iter","fakecnt","cnt","size"]
                 (tailStoreInit @(Graph h))

tailStoreLoopHeadBlock :: forall h . (Hardware h,CoreISA (Graph h))
  => (String,Int) -> BranchBlock h ((MR,MR,MR,MR,GPR,GPR,GPR,GPR)
                                   ,(MR,MR,MR,MR,GPR,GPR,GPR,GPR))
tailStoreLoopHeadBlock (label,unroll) =
  let
    tailStoreLoopHead :: forall repr . CoreISA repr
            => (repr MR,repr MR,repr MR, repr MR,repr GPR,repr GPR,repr GPR,repr GPR)
            -> (repr BRANCH,(repr MR,repr MR,repr MR, repr MR,repr GPR,repr GPR,repr GPR,repr GPR))
    tailStoreLoopHead (mrIn,mrOut,scratchIn,scratchOut,iter,fakecnt,cnt,size) =
      let
        iterM = addG cnt iter
        (v0,mrIn') = moduloDWLoad mrIn (incMR mrIn iterM) 0
        scratchIn' = moduloDWStore scratchIn (incMR scratchIn iter) 0 v0
        incr = unintegerG 8 -- Double precision
        iter' = addG incr iter
        size' = subG size incr
        branchRes = branchLow iterM size'
      in (branchRes,(mrIn',mrOut,scratchIn',scratchOut,iter',fakecnt,cnt,size))
  in branchingBlock (map toUpper label++"_TAILSTOREHEAD")
                 ["mrIn","mrOut","scratchIn","scratchOut","iter","fakecnt","cnt","size"]
                 ["brResSL","mrIn","mrOut","scratchIn","scratchOut","iter","fakecnt","cnt","size"]
                 (tailStoreLoopHead @(Graph h))

tailLoadInitBlock :: forall h . (Hardware h,CoreISA (Graph h))
  => (String,Int) -> Block h ((MR,MR,MR,MR,GPR,GPR,GPR)
                             ,(MR,MR,MR,MR,GPR,GPR,GPR))
tailLoadInitBlock (label,unroll) =
  let
    tailLoadInit :: forall repr . CoreISA repr
            => (repr MR,repr MR,repr MR, repr MR,repr GPR,repr GPR,repr GPR)
            -> (repr MR,repr MR,repr MR, repr MR,repr GPR,repr GPR,repr GPR)
    tailLoadInit (mrIn,mrOut,scratchIn,scratchOut,iter,cnt,size) =
      let
        iter' = xorG iter iter -- same as 0
      in (mrIn,mrOut,scratchIn,scratchOut,iter',cnt,size)
  in basicBlock (map toUpper label++"_TAILLOADINIT")
                 ["mrIn","mrOut","scratchIn","scratchOut","iter","cnt","size"]
                 ["mrIn","mrOut","scratchIn","scratchOut","iter","cnt","size"]
                 (tailLoadInit @(Graph h))

tailLoadLoopHeadBlock :: forall h . (Hardware h,CoreISA (Graph h))
  => (String,Int) -> BranchBlock h ((MR,MR,MR,MR,GPR,GPR,GPR)
                                   ,(MR,MR,MR,MR,GPR,GPR,GPR))
tailLoadLoopHeadBlock (label,unroll) =
  let
    tailLoadLoopHead :: forall repr . CoreISA repr
            => (repr MR,repr MR,repr MR, repr MR,repr GPR,repr GPR,repr GPR)
            -> (repr BRANCH,(repr MR,repr MR,repr MR, repr MR,repr GPR,repr GPR,repr GPR))
    tailLoadLoopHead (mrIn,mrOut,scratchIn,scratchOut,iter,cnt,size) =
      let
        iterM = addG cnt iter
        (v0,scratchOut') = moduloDWLoad scratchOut (incMR scratchOut iter) 0
        mrOut' = moduloDWStore mrOut (incMR mrOut iterM) 0 v0
        incr = unintegerG 8 -- Double precision
        iter' = addG incr iter
        size' = subG size incr
        branchRes = branchLow iterM size'
      in (branchRes,(mrIn,mrOut',scratchIn,scratchOut',iter',cnt,size))
  in branchingBlock (map toUpper label++"_TAILLOADHEAD")
                 ["mrIn","mrOut","scratchIn","scratchOut","iter","cnt","size"]
                 ["brResLL","mrIn","mrOut","scratchIn","scratchOut","iter","cnt","size"]
                 (tailLoadLoopHead @(Graph h))

{-
                            |----------------------------|
                            v                            |
 (c0)--init---(b1)--S0[0]--(c2)--loophead--(b3)--S0S1--(j4)-loopend--(c5)---S1---(r6)
               |                            |                          ^          ^
               |                            ----------------------------          |
               |                                                                  |
               |------------------------------------------------------------------|
 -}

{-

Modulo Scheudling

Assume we have 2 stages

S0
S1

we have size the size of the loop
use a local variable count

// numStages = 2
// basic block 0
count := 0
if size > 0 then
  // basic block 1
  S0[0]
  while count < size - (numStages - 1) do
    // basic block 2
    S0[count+1] ; S1[count]
    count++
  // basic block 3
  S1[count]
else
  {}
return
-}
