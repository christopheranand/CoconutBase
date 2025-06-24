-- |
-- Module      :  CoconutZ.ZScheduler
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module contains functionality to schedule a Z CodeGraph by solving a non-linear
-- optimization problem (using HashedExpression code generation with ipopt)

{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TupleSections, AllowAmbiguousTypes, OverloadedStrings #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Coconut.HashedSchedule where

import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromJust)

import qualified Data.Bifunctor

import Text.Read (readMaybe)
import qualified Data.Text as T

import System.Random
import System.FilePath ((</>))

import Data.Graph.Inductive.Graph hiding (prettify)
import Data.Graph.Inductive.Query.DFS (topsort,components,reachable)
import Data.Graph.Inductive.Query.Dominators (dom)

import HashedExpression hiding (Node)
import HashedExpression.Modeling.Typed
import qualified HashedExpression.Problem as HEProb
import qualified HashedExpression.Value as HEValue
import qualified HashedExpression.Codegen as Codegen
import HashedExpression.Codegen.CSimple

import Coconut.BaseTypes
import Coconut.Graph.CodeGraph
import Coconut.Graph.DataFlow

-- FIXME remove Debug.Trace
import Debug.Trace (trace, traceIO)
import Data.Graph.Inductive.PatriciaTree (Gr)

-- | Collection of data constructed in moduloToHashedExpressionC.
-- Primarily used to be fed to penalty and constraint generators
data HashedData h = HashedData { hdDataFlow :: DataFlowGraph h
                               , hdMetaData :: MDMap h
                               , hdII :: Double
                               , hdNumStages :: Int
                               , hdAllInstructions :: [LNode (DFNode h)]
                               , hdTVarMap :: Map Node (TypedExpr Scalar R)
                               , hdBVarMap :: Map Node (TypedExpr Scalar R)
                               , hdDepthMap :: Map Node Int
                               , hdHeightMap :: Map Node Int
                               , hdSVector :: [Double]
                               }

-- | Type synonym for @HashedData h -> TypedExpr Scalar R@
type HashedPenalty h = HashedData h -> [TypedExpr Scalar R]

-- | Type synonym for @HashedData h -> ConstraintDecl@
type HashedConstraint h = HashedData h -> [ConstraintDecl]

-- | Solver selection
data Solver = Ipopt | Glpk

-- * Generate HashedExpression Code (GLPK Bindings)
moduloToHashedExpressionGLPKB :: forall h . Hardware h
  => CodeGraph h
  -> MDMap h
  -> (String,DataFlowGraph h)
  -> Int
  -> StdGen
  -> [HashedPenalty h]
  -> [HashedConstraint h ]
  -> Solver
  -> IO (HashedData h)
moduloToHashedExpressionGLPKB cg metaData (label,dataFlow) numStages stdgen penalties constraints solver =
  let
    (CodeGraph cfGraph dataFlows cgIn cgOut mrTables constTable tags maxID dbgMap) = cg
    (DataFlowGraph dfGraph dfInps dfOuts dfOverwrites dfIncr dfStage) = dataFlow
  in error "TODO"

-- *  Generate HashedExpression Code (Using C CodeGen)

moduloToHashedExpressionC :: forall h . Hardware h
  => CodeGraph h
  -> MDMap h
  -> (String,DataFlowGraph h)
  -> Int
  -> StdGen
  -> [HashedPenalty h]
  -> [HashedConstraint h ]
  -> Solver
  -> IO (HashedData h)
moduloToHashedExpressionC cg metaData (label,dataFlow) numStages stdgen penalties constraints solver =
  let
    (CodeGraph cfGraph dataFlows cgIn cgOut mrTables constTable tags maxID dbgMap) = cg
    (DataFlowGraph dfGraph dfInps dfOuts dfOverwrites dfIncr dfStage) = dataFlow

    -- tests if a node is an instruction (NOT including InitMR's)
    isInstruction :: Node -> Bool
    isInstruction node =
      case match node dfGraph of
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

    -- NOTE this will not include InitMR's so they will not be scheduled
    -- HOWEVER this does currenlty include constants loads (i.e., "un" instructions)
    allInstructions :: [LNode (DFNode h)]
    allInstructions =
      let
        allInstrNodes = filter (isInstruction) $ topsort dfGraph
      in [ lNode | lNode <- labNodes (dfGraph), (fst lNode) `elem` allInstrNodes ]

    -- FIXME instead of filtering instruction types by name, should use metadata

    stores = filterByName (`elem` ["stv0MR"]) allInstructions
    gprs = filterByName (`elem` ["lgMR","vlgv0"]) allInstructions
    fps = filterByName (`elem` ["dfma","dfms","dfs","dfm"]) allInstructions
    vrs    = filterByName (not . (`elem` ["lgMR","vlgv0","stv0MR"])) allInstructions
    verims = filterByName (`elem` ["verim"]) allInstructions
    vlvgps = filterByName (`elem` ["vlvgp"]) allInstructions
    vlgv0s = filterByName (`elem` ["vlgv0"]) allInstructions
    lgMRs  = filterByName (`elem` ["lgMR"]) allInstructions
    vld0MRs = filterByName (`elem` ["vld0MR"]) allInstructions
    noGPRProducers = filterByName (`notElem` ["vlvgp","vlgv0","lgMR","vld0MR"]) allInstructions
    fpOps = filterByName (\n -> case n of
                             ('d':'f':_) -> True
                             _ -> False) allInstructions

    depthMap = latencyDepth dataFlow
                              (map fst allInstructions)
                              (const 1)
    latencyMap = latencyDepth dataFlow
                              (map fst allInstructions)
                              (instructionLatency dataFlow metaData)

    latencyHeightMap = latencyHeight dataFlow
                                   (map fst allInstructions)
                                   (instructionLatency dataFlow metaData)

    -------------------------------------------------------------------------------------------------
    -- Problem Configuration
    -------------------------------------------------------------------------------------------------
    numVars = length allInstructions
    -- FIXME find a better way to assign dispatch widths?
    highDispatchInstructions = map fst $ filterByName
                               (`elem` ["dfma","dfms","selb","shufb","vmah","vmal"]) allInstructions
    dispatchWidths = [ if n `elem` highDispatchInstructions then 1.5 else 1.0 |
                       n <- map fst $ allInstructions ]
    maxNumDispatchesPerCycle :: Double
    maxNumDispatchesPerCycle = 6

    -- need to compute the maximum latency between dependencies, ii must be larger then it
    -- since all dependencies must complete within an ii less duplicates become introduced
    dependencies = mkDependencies dataFlow allInstructions
    latEdges = concatMap (\(i,j) -> case (i,j) `elem` allSpillPairs of -- remove spill/despill from latGraph
                                      True -> []
                                      False -> [(i,j,latOrErr metaData dataFlow i)]
                         ) dependencies
    latNodes = concatMap (\(i,j) -> [(i,()),(j,())]) dependencies

    latGraph :: Gr () Integer
    latGraph = mkGraph latNodes latEdges

    maxPathMap = longestPathMap latGraph
    maxPathWeight s t = case (Map.lookup s maxPathMap,Map.lookup t maxPathMap) of
                          (Just w0,Just w1) -> w1 - w0
                          _ -> error $ "missing nodes in maxPathMap" ++ show (s,t)
    longestDep = maximum $ map (\(i,j) -> maxPathWeight i j) dependencies


    -- all spill resources (i.e. SpillRes nodes)
    allSpills = nodes $ labfilter isSpillRes $ dataFlowGraph dataFlow

    -- all spill / despill instruction pairs
    allSpillPairs = map spillPair allSpills
    spillPair n = case (pre (dataFlowGraph dataFlow) n ,suc (dataFlowGraph dataFlow) n) of
                    ([n0],[n1]) -> (n0,n1)
                    badSpills -> error $ "SpillRes has multiple spills/despills: " ++ show badSpills

    spillDepths = map (\(n0,n1) -> case (Map.lookup n0 maxPathMap,Map.lookup n1 maxPathMap) of
                                     (Just w0,Just w1) -> w1 - w0
                                     _ -> error $ "missing nodes in maxPathMap" ++ show (n0,n1)
                             ) allSpillPairs
    longestSpillDep = case spillDepths of
                        [] -> 0
                        xs -> maximum xs

    -- maximum latency between dependencies
    -- lets say instruction i -> j, but also has a path i -> m -> n -> j
    -- the latter path might have larger latency
    -- TODO LUCAS adjust II if regCGAllocate fail because empty prologue/epilogue
    -- if prologue is empty, ii too small, if epilogue is empty, ii to big
    iiList = [-- (sum dispatchWidths / maxNumDispatchesPerCycle)  / (fromIntegral numStages)
                   -- ,fromIntegral $ length fpOps `div` numStages
                   fromIntegral $ longestDep + 1 -- dependencies i -> j have to be j-i <= ii
                   -- ,fromIntegral $ longestSpillDep `div` 2 + 1 -- Spills/Despills have to be <= 2*ii
                   ,fromIntegral (foldr (max . snd) 0
                                  (Map.toList $ latencyHeightMap)) / (fromIntegral numStages )
                   ]

    ii =  L.maximum iiList

    tVarIdMap,bVarIdMap :: Map.Map Node String
    tVarIdMap = Map.fromList $ map (\n -> (n,"t"++show n)) $ map fst allInstructions
    bVarIdMap = Map.fromList $ map (\n -> (n,"b"++show n)) $ map fst allInstructions

    allVarIds :: [String]
    allVarIds = Map.elems tVarIdMap ++ Map.elems bVarIdMap

    tVarMap,bVarMap :: Map.Map Node (TypedExpr Scalar R)
    tVarMap = Map.map variable tVarIdMap
    bVarMap = Map.map variable bVarIdMap

    sVector :: [Double]
    sVector = genRandList stdgen $ length $ Map.toList tVarMap

    hashedData = HashedData dataFlow metaData ii numStages allInstructions tVarMap bVarMap depthMap latencyHeightMap sVector

    -------------------------------------------------------------------------------------------------
    -- Objective Function, Constraints and Initial Value Map
    -------------------------------------------------------------------------------------------------
    -- Define Objective Function
    objExpr = sum $ concatMap (\penalty -> penalty hashedData) penalties
                 -- topBottomPenalty hashedData
                 --  -- ++ ioPenalty dataFlow allInstructions tVarMap
                 --  ++ pushLoadConsumes hashedData

    -- Define Constrain Functions
    allConstraints :: [ConstraintDecl]
    allConstraints = concatMap (\constraint -> constraint hashedData) constraints
                     --    varBounds hashedData
                     -- ++ completionConstraints hashedData
                     -- ++ subjectConstraints hashedData
                     -- -- ++ constantConstraints dataFlow allInstructions tVarMap bVarMap
                     -- ++ overwriteConstraints hashedData
                     -- ++ latConstraints hashedData
                     -- ++ gprConstraints hashedData

    -- Set initial values to latency depths
    initValMap :: Map.Map String Val
    initValMap = Map.fromList $
                 -- bounds ++
                 concatMap (\(n,_) ->
                    case (Map.lookup n tVarIdMap,Map.lookup n bVarIdMap,Map.lookup n latencyMap ) of
                      (Just tVar,Just bVar, Just lat) -> [(tVar,HEValue.VNum $ fromIntegral lat)
                                                          ,(bVar,HEValue.VNum $ fromIntegral lat + eps)]
                      _ -> error $ "initValMap had bad lookup for node: " ++ show n
                           ) allInstructions
    bounds = let
        (zeroID,upID) = case (zeroBound,upperBound) of
                          (Bound z,Bound u) -> (z,u)
      in [(zeroID,HEValue.VScalar 0.0)
         ,(upID,HEValue.VScalar $ fromIntegral numStages * ii)
         ]
    -- solver directory
    solverDir =
      case solver of
        Ipopt -> "ipopt"
        Glpk -> "glpk"

    -------------------------------------------------------------------------------------------------
    -- Objective Function and Constraints
    -------------------------------------------------------------------------------------------------
    optimizationProblem = OptimizationProblem { objective = objExpr
                                              , constraints = allConstraints
                                              , values = map (uncurry (:->)) (Map.toList initValMap)
                                              }
  in do --mapM_ putStrLn debugPrint
        -- case constructProblem objExpr allConstraints of
        --   Right r -> print r
        --   Left _ -> error "No optimization problem"
        -- showExp objExpr
        putStrLn $ "iiList = " ++ show iiList
        putStrLn $ "ii = " ++ show ii
        putStrLn $ "latencyHeight = " ++ (show $ maximum $ map snd $ Map.toList latencyHeightMap)
        putStrLn $ "Num Variables = " ++ (show  $ Map.size initValMap)
        putStrLn $ "Num Constraints = " ++ (show $ length allConstraints)
        -- putStrLn $ "AllInstructions = " ++ (show allInstructions)
        writeFile (solverDir </> "src" </> (label++".hashprob"))
          $ case constructProblem objExpr allConstraints of
                                      Right p -> (show p) ++ "\n\n" ++ show initValMap
                                      Left e -> e
        proceed optimizationProblem
                CSimpleConfig { output = OutputCSV,  maxIteration = Nothing }
                (solverDir </> "src")
        writeFile (solverDir </> "src" </> (label++".ii")) (show ii)
        writeFile (solverDir </> "src" </> (label++".rand")) (show sVector)
        writeFile (solverDir </> "src" </> (label++".stdgen")) (show stdgen)
        return hashedData


instance Show Problem where
  show Problem {..} =
    L.intercalate
    "\n"
    ["\n-------------------------------------------------------------------------------",
    showObjective,
--    showPartials,
    L.intercalate "\n" (map showGeneralConstraint generalConstraints)
    ]
    where
      showObjective = "Objective: " ++ prettify (expressionMap, objectiveId)
      showPartials =
        L.intercalate "\n" $
        ["∂/∂" ++ varName var ++ ": " ++ prettify (expressionMap, partialDerivativeId var)
        | var <- variables]
      showGeneralConstraint (GeneralConstraint vId pIDs lb ub) =
          let
            withVarName = zip (map varName variables) pIDs
          in -- L.intercalate "\n" $
             ("Constraint: " ++ show lb ++ " <= " ++ prettify (expressionMap, vId) ++ " <= " ++ show ub)
             -- : ["∂/∂" ++ name ++ ": " ++ prettify (expressionMap, pID) | (name, pID) <- withVarName]

--------------------------------------------------------------------------------------------------------------------------------------------
--  Hard Constraints
--------------------------------------------------------------------------------------------------------------------------------------------
eps = 0.3333
fauxTop = 1.0e8

zeroBound :: Bound Scalar
zeroBound = Bound "zeroBound"
upperBound :: Bound Scalar
upperBound = Bound "upperBound"


-- TODO make these an actual Bound not ConstraintDecl?
varBounds :: Hardware h
          => HashedData h
          -> [ConstraintDecl]
varBounds hashedData =
  let
    numStages = hdNumStages hashedData
    instrs = hdAllInstructions hashedData
    ii = hdII hashedData
    tVar = hdTVarMap hashedData
    bVar = hdBVarMap hashedData
  in concatMap (\n -> [(tVar Map.! n) .>= (0.0 :: Double)
                      ,(bVar Map.! n) .>= (0.0  :: Double)
                      ,(tVar Map.! n) .<= (fromIntegral numStages * ii)
                      ,(bVar Map.! n) .<= (fromIntegral numStages * ii)
                      ]) $ map fst instrs
  -- in concatMap (\n -> [(tVar Map.! n) .>= zeroBound
  --                     ,(bVar Map.! n) .>= zeroBound
  --                     ,(tVar Map.! n) .<= upperBound
  --                     ,(bVar Map.! n) .<= upperBound
  --                     ]) $ map fst instrs
subjectConstraints
  :: Hardware h
     => HashedData h
     -> [ConstraintDecl]
subjectConstraints hashedData =
  let
    dfGraph = hdDataFlow hashedData
    instrs = hdAllInstructions hashedData
    ii = hdII hashedData
    tVar = hdTVarMap hashedData
    bVar = hdBVarMap hashedData
    -- find output registers
    fglGraph = dataFlowGraph dfGraph
    (finalInstructions:_ ) =
      nodes
      $ gfiltermap (\ctx -> case ctx of
                       (_,_,_,[]) -> Nothing -- filter out output regs
                       (_,_,_,succs) -> -- succs should be all output regs (have no succs themselves)
                                              -- meaning final instr
                         case concatMap (\s -> suc fglGraph s) (map snd succs) of
                           [] -> Just ctx
                           _ -> Nothing
                   ) fglGraph
    isSpillNode n = isSpill $ fromJust $ lab fglGraph n
    deps = mkDependencies dfGraph instrs
    depConstraints = concatMap (\(i,j) ->
                                  [(tVar Map.! j - tVar Map.! i) .>= eps
                                  ,(bVar Map.! i - tVar Map.! j) .>= ( 0.0 :: Double)
                                  ]) deps
    -- NOTE we use FIFOs to allow spill nodes to span more than II, opening up the schedule space
    iiConstraints = map (\(i,j) ->
                           case isSpillNode i of
                             True -> (tVar Map.! j - tVar Map.! i) .<= (2*ii-eps)
                             False -> (tVar Map.! j - tVar Map.! i) .<= (ii-eps)
                         ) deps
  in depConstraints ++ iiConstraints

finalInstrConstraint
  :: Hardware h
     => HashedData h
     -> [ConstraintDecl]
finalInstrConstraint hashedData =
  let
    dfGraph = hdDataFlow hashedData
    instrs = hdAllInstructions hashedData
    ii = hdII hashedData
    tVar = hdTVarMap hashedData
    bVar = hdBVarMap hashedData
    numStages = hdNumStages hashedData

    -- find output registers
    fglGraph = dataFlowGraph dfGraph
    topSortedInstrs = filter(\n -> n `elem` (map fst instrs)) $ topsort fglGraph
    finalInstruction = last topSortedInstrs
    firstinstruction = head topSortedInstrs
    -- (finalInstruction:_ ) =
    --   nodes
    --   $ gfiltermap (\ctx -> case ctx of
    --                    (_,_,_,[]) -> Nothing -- filter out output regs
    --                    (_,_,_,succs) -> -- succs should be all output regs (have no succs themselves)
    --                                           -- meaning final instr
    --                      case concatMap (\s -> suc fglGraph s) (map snd succs) of
    --                        [] -> Just ctx
    --                        _ -> Nothing
    --                ) fglGraph
  in [tVar Map.! firstinstruction .== (0.0 :: Double)
     ,tVar Map.! finalInstruction .== (fromIntegral numStages*ii - eps)]

-- FIXME using this creats an unsolvable optimization problem
constantConstraints :: Hardware h
     => HashedData h
     -> [ConstraintDecl]
constantConstraints hashedData =
  let
    dfGraph = hdDataFlow hashedData
    instructions = hdAllInstructions hashedData
    tVar = hdTVarMap hashedData
    bVar = hdBVarMap hashedData

    allNodes = labNodes $ dataFlowGraph dfGraph
    allEdges = labEdges $ dataFlowGraph dfGraph
    constants = filterByName (\name -> case name of
                                         'u':'n':_ -> True
                                         _ -> False) allNodes
    loadConsumerPairs = concatMap (\n -> zip (repeat n) $ instructionConsumers dfGraph n)
                        $ map fst constants
  -- NOTE because of subjectConstraints, this causes tVar[j] - tVar[i] == eps
  in map (\(i,j) -> (tVar Map.! j - tVar Map.! i) .<= (eps)) loadConsumerPairs

completionConstraints :: Hardware h
                      => HashedData h
                      -> [ConstraintDecl]
completionConstraints hashedData =
  let
    instructions = hdAllInstructions hashedData
    tVar = hdTVarMap hashedData
    bVar = hdBVarMap hashedData
  in map ((\n -> (bVar Map.! n - tVar Map.! n) .>= eps) . fst) instructions

overwriteConstraints :: Hardware h
                     => HashedData h
                     -> [ConstraintDecl]
overwriteConstraints hashedData =
  let
    dfGraph = hdDataFlow hashedData
    ii = hdII hashedData
    tVar = hdTVarMap hashedData

    -- filter MR's out of overwrites (they shouldn't be an issue)
    overwrites = filter (\(_,(n0,_)) -> case matchResType n0 dfGraph of
                            Just (MemoryRes _) -> False
                            Just _ -> True
                            Nothing -> error $ "bad overwrite in dataflow graph: " ++ show n0)
                 $ dataFlowOverwrites dfGraph
  in concatMap (overwriteConstraint dfGraph ii tVar) overwrites

overwriteConstraint :: Hardware h
                     => DataFlowGraph h
                     -> Double
                     -> Map.Map Node (TypedExpr Scalar R)
                     -> (Node,(Node,Node))
                     -> [ConstraintDecl]
overwriteConstraint dfGraph ii tVar (instr,(reg,reg')) =
  let
    lEdges = labEdges (dataFlowGraph dfGraph)
    -- if in a later stage, postReads need to come before preWrites
    -- a postRead will be an instruction that contains the post-overwritten reg node (i.e., reg')
    postReads = L.nub $ concatMap (\(n0,n1,_) -> if n0 == reg' then [n1] else []) lEdges
    -- a preWrite will be an instruction that contains the pre-overwritten reg node (i.e., reg)
    preWrites = L.nub $ concatMap (\(n0,n1,_) -> if n1 == reg then [n0] else []) lEdges

    -- a preRead will be an instruction that contains the pre-overwritten reg node (i.e., reg)
    preReads = L.nub $ concatMap (\(n0,n1,_) -> if n0 == reg && n1 /= instr then [n1] else []) lEdges
  in -- instructions that postRead need to come before preWrites in the modulo loop
    concatMap (\r -> map (\w -> (tVar Map.! r - tVar Map.! w) .<= (ii-eps)) preWrites) postReads
    -- instructions that preRead need to come before node is ovewritten
   ++ map (\r -> (tVar Map.! instr - tVar Map.! r) .>= eps) preReads

-- FIXME switch this to penalty?
latConstraints :: (Hardware h)
               => HashedData h
               -> [ConstraintDecl]
latConstraints hashedData = let
  dfGraph = hdDataFlow hashedData
  instructions = hdAllInstructions hashedData
  metaData = hdMetaData hashedData
  tVar = hdTVarMap hashedData

  -- TODO move me into Hardware typeclass and do a more thorough job identifying stores/loads
  -- NOTE not including spills in here will make problem infeasible?
  notLoadStore name = case name of
                        "lgMR" -> False
                        ('s':'p':'i':'l':'l':_) -> False
                        ('d':'e':'s':'p':'i':'l':'l':_) -> False
                        -- "vlgv0" -> False
                        -- "stv0MR" -> False
                        ('u':'n':_) -> False
                        _ -> True
  vrs    = filterByName notLoadStore instructions
    -- filterByName (not . (`elem` ["lgMR","vlgv0","stv0MR"])) instructions
  -- lat1 :: [(Node,Node,Int)]
  lat1 = map (\(i,j) -> (i,j,latOrErr metaData dfGraph i)) $ mkDependencies dfGraph vrs
  completionDepth :: Double
  completionDepth = 1
  deepen lats = concatMap (\(i,j,depth) ->
                             if isStore dfGraph j
                             then []
                             else [(i,k,depth + latOrErr metaData dfGraph j) | k <- instructionConsumers dfGraph j]) lats

  latencyPairs = L.nubBy ( \(i,j,_) (l,m,_) -> i==l && j==m )
                 $ L.reverse
                 $ L.sort
                 $ filter (\(_,_,depth) -> depth > completionDepth)
                 -- $ deepen $ deepen $ deepen lat1
                 $ deepen lat1

  in (map (\(i,j,depth) ->
            (tVar Map.! j - tVar Map.! i) .>=
              (depth - completionDepth))
      latencyPairs)
     -- `debug` ("\nlatencyPairs: " ++ show latencyPairs ++ "\n")

-- TODO space gpr components apart by fst - third, second - fourth so there can be some overlap
-- find components of just gpr operations that need to be spaced out from one
-- another in order to register alocate
gprConstraints :: forall h . Hardware h
               => HashedData h
               -> [ConstraintDecl]
gprConstraints hashedData =
  let
    dfGraph = hdDataFlow hashedData
    allInstructions = hdAllInstructions hashedData
    ii = hdII hashedData
    depthMap = hdDepthMap hashedData
    tVar = hdTVarMap hashedData

    minComponentSize = 5
    isInstructionOrGPR dfNode = case dfNode of
                                  ResourceNode (RegisterRes regType) ->  isGPR regType
                                  ResourceNode (MemoryRes _) -> False
                                  ResourceNode EmptyRes -> False
                                  _ ->  True
    isInstruction dfNode = case dfNode of
                                  ResourceNode _ ->  False
                                  _ -> True
    isNotInitMR dfNode = case dfNode of
                           InstructionNode (InitMR _ _) -> False
                           _ -> True
    gprGraph = labnfilter (\(_,dfNode) -> isInstructionOrGPR dfNode && isNotInitMR dfNode)
               $ dataFlowGraph dfGraph
    gprComponents = filter ((>=minComponentSize) . length) $ components gprGraph

    isStartReg n = case match n gprGraph of
                        (Just ([],_,_,_),_) -> True
                        _ -> False
    isStartInstr n = case match n gprGraph of
                        (Just (adjPrevs,_,_,_),_) -> (n `elem` (map fst allInstructions) )
                                                     && and (map (isStartReg . snd) adjPrevs)
                        _ -> False
    isFinalReg n = case match n gprGraph of
                        (Just (_,_,_,[]),_) -> True
                        _ -> False
    isFinalInstr n = case match n gprGraph of
                        (Just (_,_,_,adjSuccs),_) -> (n `elem` (map fst allInstructions) )
                                                     && and (map (isFinalReg . snd) adjSuccs)
                        _ -> False
    sortComponents cs = map fst
                        $ L.sortOn snd
                        $ map (\(c0,c1) -> ((c0,c1), minimum $ map dMapLookup c0) ) cs
    startFinalPairs = sortComponents $
                      filter (\(s0,s1) -> not (s0 == s1)) $
                      map (\compNodes -> (filter isStartInstr compNodes
                                         ,filter isFinalInstr compNodes)) gprComponents
    gprConstraint ((sNodes0,fNodes0),(sNodes1,fNodes1))
       | componentsAreDisconnected dfGraph (sNodes0,fNodes0) (sNodes1,fNodes1) =
           [ (tVar Map.! s1 - tVar Map.! f0) .>= eps | s1 <- sNodes1, f0 <- fNodes0 ] ++
           [ (tVar Map.! f1 - tVar Map.! s0) .<= (ii-eps) | s0 <- sNodes0, f1 <- fNodes1 ]
       | otherwise = []

    -- TODO calculate latency height/depth of components and filter out ones with large difference?
    dMapLookup n = case Map.lookup n depthMap of
                     Just d -> d
                     Nothing -> error $ "bad lookup in depthMap: "++show n
    maxDepth = maximum $ map snd $ Map.toList depthMap

    componentIsTooLarge (sNodes,fNodes) = let
        minSNode = minimum $ map dMapLookup sNodes
        maxSNode = maximum $ map dMapLookup fNodes
      in maxSNode - minSNode >= maxDepth `div` 4
        -- fNode1 - ii <= sNode0
        -- fNode1 - sNode0 <= ii
    genGPRConstraints (c0:c1:cs)
      | componentsAreDisconnected dfGraph c0 c1 = gprConstraint (c0,c1) ++ genGPRConstraints (c1:cs)
      | otherwise = genGPRConstraints (c1:cs)
    genGPRConstraints  _ = []
    -- genGPRConstraints (c0:c1:c2:cs)
    --   | componentsAreDisconnected dfGraph c0 c2 = gprConstraint (c0,c2) ++ genGPRConstraints (c1:c2:cs)
    --   | otherwise = genGPRConstraints (c1:c2:cs)
    -- genGPRConstraints  _ = []

    gprConstraints = genGPRConstraints startFinalPairs-- concatMap gprConstraint componentPairs

    componentPairs = removeRedundantPairs $
                     [ (c0,c1) | c0 <- startFinalPairs, c1 <- (tail startFinalPairs)
                               , c0 /= c1
                               , componentsAreDisconnected dfGraph c0 c1 ]
    removeRedundantPairs ((c0,c1):ps) = (c0,c1) : removeRedundantPairs (filter (\p -> p /= (c1,c0)) ps)
    removeRedundantPairs [] = []

  in gprConstraints
      -- `debug` ("number gpr constraints: " ++ (show $ length gprConstraints)
      --                       ++"\nstartFinalPairs: " ++ (show startFinalPairs)
      --                       ++"\ncomonentPairs" ++ show (componentPairs))

componentsAreDisconnected :: Hardware h => DataFlowGraph h -> ([Node],[Node]) -> ([Node],[Node]) -> Bool
componentsAreDisconnected dfGraph (sNodes0,fNodes0) (sNodes1,fNodes1) =
  let
    -- n1 is reachable from n0
    isReachable n0 n1 = n1 `elem` (reachable n0 $ dataFlowGraph dfGraph)
    -- the starting nodes of comp0 are reachable from the final nodes of comp1
    comp0Wraps1 = (or [ isReachable sNode0 sNode1 | sNode0 <- sNodes0, sNode1 <- sNodes1 ])
                && (or [ isReachable sNode1 fNode0 | fNode0 <- fNodes0, sNode1 <- sNodes1 ])
    -- the starting nodes of comp1 are reachable from the final nodes of comp0
    comp1Wraps0 = (or [ isReachable sNode1 sNode0 | sNode0 <- sNodes0, sNode1 <- sNodes1 ])
                && (or [ isReachable sNode0 fNode1 | fNode1 <- fNodes1, sNode0 <- sNodes0 ])
    -- components are disconnected if neither component is reachable from the other
  in not (comp0Wraps1 || comp1Wraps0)


-- spillConstraints :: Hardware h => HashedData h -> [ConstraintDecl]
-- spillConstraints hashedData =
--   let
--     tVar = hdTVarMap hashedData
--     dfGraph = dataFlowGraph $ hdDataFlow hashedData
--     ii = hdII hashedData
--     allInstructions = hdAllInstructions hashedData

--     -- find all the (spill,despill) node pairs
--     allSpillPairs = map spillPair allSpills
--     allSpills = filter (\(n,dfNode) -> isSpill dfNode) allInstructions
--     spillPair (n0,spillNode) =
--       case concatMap (suc dfGraph) $ suc dfGraph n0 of
--         (n1:_) -> case match n1 dfGraph of
--                      (Just (_,_,InstructionNode (Despill _),_),_) -> (n0,n1)
--                      _ -> error $ "spill consumed by instruction other than despill" ++ show (n0,n1)
--         _ -> error $ "bad spill without despill " ++ show n0

--     -- find the instruction that produces a spill and pair it with all that consume the despill
--     allProdConsPairs = concatMap producerConsumerPairs allSpillPairs
--     producerConsumerPairs (nSpill,nDespill) =
--       case instructionProducers (hdDataFlow hashedData) nSpill of
--         [n] -> [ (n,n0) | n0 <- instructionConsumers (hdDataFlow hashedData) nDespill ]
--         -- should only be one producer
--         _ -> error $ "spill has multiple producers: " ++ show nSpill

--     -- (consumer(despill) - producer(spill)) < ii
--   in map (\(n0,n1) -> (tVar Map.! n1 - tVar Map.! n0) .<= (ii-eps)) allProdConsPairs

spillConstraints :: Hardware h => HashedData h -> [ConstraintDecl]
spillConstraints hashedData =
  let
    tVar = hdTVarMap hashedData
    dfGraph = hdDataFlow hashedData
    ii = hdII hashedData
    allInstructions = hdAllInstructions hashedData

    -- all spill resources (i.e. SpillRes nodes)
    allSpills = nodes $ labfilter isSpillRes $ dataFlowGraph dfGraph

    -- all spill / despill instruction pairs
    allSpillPairs = map spillPair allSpills
    spillPair n = case (pre (dataFlowGraph dfGraph) n ,suc (dataFlowGraph dfGraph) n) of
                    ([n0],[n1]) -> (n0,n1)
                    badSpills -> error $ "SpillRes has multiple spills/despills: " ++ show badSpills

    -- all pairs of instructions that produce a spilled reg and consume the corresponding despilled reg
    allProdConsPairs = concatMap (\(n0,n1) -> [ (p,c) | p <- instructionProducers dfGraph n0
                                                      , c <- instructionConsumers dfGraph n1 ]
                                 ) allSpillPairs

  in map (\(n0,n1) -> (tVar Map.! n1 - tVar Map.! n0) .<= (ii-eps)) allProdConsPairs

-- added to subjectConstraints
-- fifoSpillConstraints :: Hardware h => HashedData h -> [ConstraintDecl]
-- fifoSpillConstraints hashedData =
--   let
--     tVar = hdTVarMap hashedData
--     dfGraph = hdDataFlow hashedData
--     ii = hdII hashedData
--     allInstructions = hdAllInstructions hashedData

--     -- all spill resources (i.e. SpillRes nodes)
--     allSpills = nodes $ labfilter isSpillRes $ dataFlowGraph dfGraph
--     -- all spill / despill instruction pairs
--     allSpillPairs = map spillPair allSpills
--     spillPair n = case (pre (dataFlowGraph dfGraph) n ,suc (dataFlowGraph dfGraph) n) of
--                     ([n0],[n1]) -> (n0,n1)
--                     badSpills -> error $ "SpillRes has multiple spills/despills: " ++ show badSpills

--   in map (\(n0,n1) -> (tVar Map.! n1 - tVar Map.! n0) .<= (2*ii-eps)) allSpillPairs

--------------------------------------------------------------------------------------------------------------------------------------------
--  Penalties
--------------------------------------------------------------------------------------------------------------------------------------------

gaussFieldPenalty :: Hardware h
                  => HashedData h
                  -> [TypedExpr Scalar R]
gaussFieldPenalty hashedData =
  let
    dfGraph = hdDataFlow hashedData
    roots = map snd $ dataFlowInputs dfGraph
    instructions = map fst $ hdAllInstructions hashedData
    tVars = hdTVarMap hashedData
    ii = hdII hashedData

    depthMap = hdDepthMap hashedData

    -- sum_i sum_j where i /= j and not (i elem dom(j)), e ()
  in concatMap (\i ->
                  concatMap (\j -> let
                                (iDepth,jDepth) = case (Map.lookup i depthMap,Map.lookup j depthMap) of
                                                    (Just i0,Just j0) -> (i0,j0)
                                                    _ -> error "gaussFieldPenalty instruction missing from depthMap"
                             in if i == j || jDepth < iDepth
                                   then []
                                   else [1000000*exp (-10*(tVars Map.! j - tVars Map.! i) HashedExpression.^ 2)
                                        ,1000000*exp (-10*(tVars Map.! j - tVars Map.! i - fromDouble ii) HashedExpression.^ 2)]
                            )
                      $ instructions
                  ) instructions

genRandList :: StdGen -> Int -> [Double]
genRandList stdgen n
  | n <= 0    = []
  | otherwise = let
      (x,stdgen') = randomR (-1,1) stdgen
    in x : genRandList stdgen' (n-1)

stochasticPenalty :: Hardware h
                  => TypedExpr Scalar R
                  -> HashedData h
                  -> [TypedExpr Scalar R]
stochasticPenalty range hashedData =
  let
    sVector = hdSVector hashedData
    tVars = hdTVarMap hashedData
    tVarsSorted = map ((range*) . snd) $ Map.toAscList tVars
  in zipWith (*) (map fromDouble sVector) tVarsSorted

-- pullParams :: StdGen -> Map.Map Edge (TypedExpr Scalar R) -> Map.Map Edge (TypedExpr Scalar R) -> [TypedExpr Scalar R]
-- pullParams stdgen tVars bVars =
--   let
--     pulls = map ((100*) . fromIntegral) (genRandList stdgen $ fromIntegral $ Map.size tVars)
--   in zipWith (curry (\(pull,(tVar,bVar)) -> pull * tVar + pull * bVar)) pulls (zip (Map.elems tVars) (Map.elems bVars))

-- FIXME this ioPenalty is missing the indicator function (and actual latencies of consumers)
ioPenalty :: Hardware h =>
  HashedData h -> [TypedExpr Scalar R]
ioPenalty hashedData =
  let
    dfGraph = hdDataFlow hashedData
    instructions = hdAllInstructions hashedData
    tVar = hdTVarMap hashedData

    vrs      = filterByName (not . (`elem` ["lgMR","vlgv0","stv0MR"])) instructions
    numConsumedRegs n = length $ pre (dataFlowGraph dfGraph) n
    numProducedRegs n = length $ suc (dataFlowGraph dfGraph) n

    ioDelta n = fromIntegral $ numConsumedRegs n - numProducedRegs n
    scale = 10000000

    -- numConsumers dfGraph n = length $ instructionConsumers dfGraph n
    -- numProducers dfGraph n = length $ instructionProducers dfGraph n
    -- io n = [(n,1 - sum (map (1 /)
    --                 $ filter (>0)
    --                 $ map (fromIntegral . numConsumers dfGraph)
    --                 $ filter (isVR dfGraph)
    --                 $ instructionProducers dfGraph n))]
  in -- map (\(v,w) -> w * (tVar Map.! v)) $ concatMap (io . fst) vrs
     map (\(n,_) -> scale * ioDelta n * tVar Map.! n) vrs

topBottomPenalty :: Hardware h
  => ((Node,DFNode h) -> TypedExpr Scalar R)
  -> HashedData h
  -> [TypedExpr Scalar R]
topBottomPenalty scale hashedData =
  let
    dfGraph = hdDataFlow hashedData
    instructions = hdAllInstructions hashedData
    tVars = hdTVarMap hashedData
    bVars = hdBVarMap hashedData

    consumesConstant (n,dfNode) = or
                                  $ map (\(_,dfNode') -> isConstLoad dfNode')
                                  $ instructionProducersL dfGraph n
    -- the instructions consumer also consumes a constant load
    hasConstantNeighbour n
      | isConstLoadDF dfGraph n = False
      | otherwise = or $ map consumesConstant $ instructionConsumersL dfGraph n

    -- remove spills
    instructions' = filter (not . isSpill . snd) instructions
    -- scale (n,dfNode)
    --   | isConstLoad dfNode = 100000
    --   | hasConstantNeighbour n = -100000
    --   | otherwise = 100
    -- scale (n,dfNode) = 10000
  in map (\(n,dfNode) -> scale (n,dfNode) * (bVars Map.! n - tVars Map.! n)) instructions'

pushLoadConsumes :: Hardware h
  => HashedData h
  -> [TypedExpr Scalar R]
pushLoadConsumes hashedData =
  let
    dataFlow = hdDataFlow hashedData
    dfGraph= dataFlowGraph dataFlow
    instructions = hdAllInstructions hashedData
    tVars = hdTVarMap hashedData

    -- find all lgMR nodes
    lgMRs = filter (\(n,dfNode) -> dfNodeHasName dfNode (=="lgMR")) instructions
    -- find all vlvgp's that consume a given node
    findVlvgpConsumer (n,dfNode) = filter (\(n,dfNode) -> dfNodeHasName dfNode (=="vlvgp"))
                                   $ instructionConsumersL dataFlow n
    -- make pairs of lgMRs and vlvgp consumers
    lgMR_vlvgps = concatMap (\(n0,dfNode0) -> map (\(n1,_) -> (n0,n1))
                                              $ findVlvgpConsumer (n0,dfNode0)) lgMRs
    scale = -10000
  in map (\(n0,n1) -> scale * (tVars Map.! n1 - tVars Map.! n0)) lgMR_vlvgps

spillPenalty :: HashedData h -> [TypedExpr Scalar R]
spillPenalty hashedData =
  let
    dataFlow = hdDataFlow hashedData
    dfGraph= dataFlowGraph dataFlow
    tVars = hdTVarMap hashedData

    -- all spill resources (i.e. SpillRes nodes)
    allSpills = nodes $ labfilter isSpillRes dfGraph

    -- all spill / despill instruction pairs
    allSpillPairs = map spillPair allSpills
    spillPair n = case (pre dfGraph n ,suc dfGraph n) of
                    ([n0],[n1]) -> (n0,n1)
                    badSpills -> error $ "SpillRes has multiple spills/despills: " ++ show badSpills
    scale = -1000000
  in map (\(n0,n1) -> scale * (tVars Map.! n1 - tVars Map.! n0)) allSpillPairs

storePenalty hashedData =
  let
    dataFlow = hdDataFlow hashedData
    dfGraph= dataFlowGraph dataFlow
    tVars = hdTVarMap hashedData

    allInstrs = hdAllInstructions hashedData
    allStores = filterByName (=="stvxMR") allInstrs
    scale = 10000000
  in map (\(n,_) -> scale * tVars Map.! n) allInstrs

-- * Utilities

-- | Filter instructions (all @LNode (DFNode h)@ are assumed to be instructions)
-- based on a given predicate over the instruction name
filterByName :: (String -> Bool) -> [LNode (DFNode h)] -> [LNode (DFNode h)]
filterByName p nodes =
  let
    compareName lNode = case lNode of
                        (_,InstructionNode el) ->
                          case el of
                            Instruction _ name _ -> p name
                            Move name _ -> p name
                            InitMR _ _ -> p "initMR"
                            _ -> False
                        (_,BranchNode name _ _) -> p name
                        _ -> False
  in filter compareName nodes

-- | Given a node in a dataflow graph, test if a given instruction's name
-- satisfies a given predicate p
instrHasName :: DataFlowGraph h -> Node -> (String -> Bool) -> Bool
instrHasName dfGraph n p = case match n (dataFlowGraph dfGraph ) of
  (Just (_,_,InstructionNode el,_),_) -> case el of
                              Instruction _ name _ -> p name
                              Move name _ -> p name
                              _ -> False
  _ -> False

-- | Given a @DFNode@, test if a given instruction's name satisfies a given
-- predicate p
dfNodeHasName :: DFNode h -> (String -> Bool) -> Bool
dfNodeHasName dfNode p = case dfNode of
  InstructionNode el -> case el of
                          Instruction _ name _ -> p name
                          Move name _ -> p name
                          _ -> False
  _ -> False

-- TODO isVR should be handled by metadata
-- | Test if a given instruction returns a @VR@
isVR :: DataFlowGraph h -> Node -> Bool
isVR dfGraph n = instrHasName dfGraph n (`notElem` ["lgMR","vlgv0","stv0MR"])

-- | Test if a given instruction is a @stv0MR@
isStore :: DataFlowGraph h -> Node -> Bool
isStore dfGraph n = instrHasName dfGraph n (\n -> n=="stv0MR" || n=="stvxMR" || n=="moduloVStore")

-- | Test if a given instruction is a constant load
isConstLoadDF :: DataFlowGraph h -> Node -> Bool
isConstLoadDF dfGraph n = instrHasName dfGraph n (\name -> case name of
                                                     'u':'n':_ -> True
                                                     _ -> False)

-- | Test if a given instruction is a constant load
isConstLoad :: DFNode h -> Bool
isConstLoad dfNode = case dfNode of
      InstructionNode el ->
        case el of
            Instruction _ name _ ->
              case name of
                    'u':'n':_ -> True
                    _ -> False
            _ -> False
      _ -> False

-- | Test if a given instruction is a spill
isSpill :: DFNode h -> Bool
isSpill dfNode = case dfNode of
      InstructionNode el ->
        case el of
            Spill _ -> True
            _ -> False
      _ -> False

-- | Test if a given instruction is an despill
isDespill :: DFNode h -> Bool
isDespill dfNode = case dfNode of
      InstructionNode el ->
        case el of
            Despill _ -> True
            _ -> False
      _ -> False

isSpillRes :: DFNode h -> Bool
isSpillRes dfNode = case dfNode of
      ResourceNode resType ->
        case resType of
            SpillRes -> True
            _ -> False
      _ -> False

-- TODO find better place to put the below code

-- -- NOTE: filters dependencies to be closed under the input edges
mkDependencies :: Hardware h => DataFlowGraph h -> [LNode (DFNode h)] -> [(Node,Node)]
mkDependencies dfGraph instrs =
  let
    nodes = map fst instrs
    isValidDep (e1,e2) = e1 `elem` nodes && e2 `elem` nodes
  in filter isValidDep
     $ concatMap (\n -> map (\n' -> (n,n')) (instructionConsumers dfGraph n)) nodes

latOrErr metaData dfGraph n = latencyLookup metaData $ case matchDFNode n dfGraph of
                            Just (ResourceNode res) -> error $ "latencyLookup given ResourceNode: " ++ show n
                            Just dfNode -> dfNode
                            Nothing -> error $ "latOrErr given node missing in dataflow graph: " ++ show n
latencyLookup metaData (ResourceNode res) = error $ "latencyLookup given ResourceNode"
latencyLookup metaData (BranchNode name _ _) =
  case Map.lookup name (mdMap metaData) of
    Just mData -> fromIntegral $ hardwareLatency mData
    Nothing -> error $ "missing instruction in metaData: " ++ name
latencyLookup metaData (InstructionNode (Instruction _ name _)) =
  case Map.lookup name (mdMap metaData) of
    Just mData -> fromIntegral $ hardwareLatency mData
    Nothing -> error $ "missing instruction in metaData: " ++ name
latencyLookup metaData (InstructionNode _) = 0

-- | Compute a @Map@ from every node in its graph to the weighting of its
-- longest path. This method uses a linear algorithm based on first doing a
-- topological sort NOTE you can use this map to find the maximum path by
-- traversing the graph in reverse, picking the predessor with the maximum cost
-- at each point.
longestPathMap :: (DynGraph gr, Num a1, Ord a1) => gr a2 a1 -> Map Node a1
longestPathMap graph =
  let
    nodes = topsort graph
  in computeMaxMap nodes graph Map.empty

-- | Given a list of topologically sorted nodes in a graph, compute map of each
-- node to their maximum path cost
computeMaxMap :: (DynGraph gr, Num a1, Ord a1) =>
                 [Node] -> gr a2 a1 -> Map Node a1 -> Map Node a1
computeMaxMap []     graph maxMap = maxMap
computeMaxMap (n:ns) graph maxMap =
  let
    maxLookup n0 = case Map.lookup n0 maxMap of
                     Just m0 -> m0
                     Nothing -> error $ "computeMaxMap failed to lookup a node in maxMap: "
                                       ++ show n0

    maxPreNode = case lpre graph n of
                   [] -> 0
                   xs -> maximum $ map (\(n,w) -> maxLookup n + w) xs

    maxMap' = Map.insert n maxPreNode maxMap

  in computeMaxMap ns graph maxMap'
