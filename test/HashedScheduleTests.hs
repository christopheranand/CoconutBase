-- |
-- Module      :  test.HashedScheduleTests
-- Copyright   :  (c) OCA 2021
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module is the test-suite for scheduling codegraphs with HashedSchedule

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module HashedScheduleTests where

import qualified Data.Graph.Inductive as FGL

import Coconut.BaseTypes
import Coconut.Utils.CGWrappers
import Coconut.Graph.DataFlow
import Coconut.Graph.ControlFlow
import Coconut.Graph.CodeGraph
import Coconut.Core.CoreISA
import Coconut.Core.ControlISA (compose,branch,doWhile)
import Coconut.Core.CoreHardware (CORE)
import Coconut.Core.MetaData

import Coconut.RegisterAllocator
import Coconut.Schedule
import Coconut.Graph.Dot
import Coconut.Simulator
import Coconut.HashedSchedule

import HashedExpression hiding (Node)
import HashedExpression.Modeling.Typed

import Data.ByteString (ByteString, putStrLn)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap as IntMap
import Data.Either (fromRight)
import Data.IntMap (IntMap)
import GHC.TypeNats (KnownNat)
import Data.Maybe (fromJust)
import qualified Data.List as List
import System.Process
import System.Random
import Coconut.CodeGen
import Data.Bifunctor
import Coconut.Rewrite (cgApplyRewrites)
import Data.Char
import Data.Graph.Inductive.Query.DFS (topsort)
import AddChains

--------------------------------------------------------------------------------------------
-- Test Cases
--------------------------------------------------------------------------------------------

-- * Generic test cases

testCond :: forall repr. CoreISA repr => (repr GPR,repr GPR) -> (repr BRANCH,(repr GPR,repr GPR))
testCond (g0,g1) =
  let
    g3 = unintegerG 1
    g4 = addG g0 g3
    g5 = cgrjnl g0 g1
  in (g5,(g4,g1))

testA :: forall repr . CoreISA repr => (repr GPR,repr GPR) -> repr GPR
testA (g4,g1) =
  let
    g6 = addG g4 g1
  in g6

testB :: forall repr . CoreISA repr => (repr GPR,repr GPR) -> repr GPR
testB (g4,g1) =
  let
    g7 = addG g4 g1
    g8 = addG g7 g1
  in g8

testPost :: forall repr . CoreISA repr => repr GPR -> repr GPR
testPost g6_g8 =
  let
    g9 = negG g6_g8
  in g9

testBranch :: forall h . (Hardware h, CoreISA (Graph h)) => Block h ((GPR,GPR),GPR)
testBranch =
  let
    cBlock = branchingBlock "condBlock" ["g0","g1"] ["g5","g4","g1"] testCond
    aBlock = basicBlock "aBlock" ["g4","g1"] ["g6"] $ testA @(Graph h)
    bBlock = basicBlock "bBlock" ["g4","g1"] ["g8"] $ testB @(Graph h)
    pBlock = basicBlock "postBlock" ["g6_g8"] ["g9"] $ testPost @(Graph h)
  in branch cBlock aBlock bBlock pBlock

testBlockC :: forall repr . CoreISA repr => repr GPR -> repr GPR
testBlockC g0 =
  let
    g1 = addG g0 g0
    g2 = addG g0 g0
    g3 = addG g1 g2
  in g3

testCodeGraph :: CodeGraph CORE
testCodeGraph = createCG (genBlock testBranch)

testBasicBlockA :: CodeGraph CORE
testBasicBlockA = createCG (genBlock (basicBlock "aBlock" ["g4","g1"] ["g6"] (testA @(Graph CORE))))

testBasicBlockB :: CodeGraph CORE
testBasicBlockB = createCG (genBlock (basicBlock "bBlock" ["g4","g1"] ["g6"] (testB @(Graph CORE))))

genQuadTests = do testHashedCodeGen testQuadCG "quadtest"
                  testHashedInterferenceGraph testQuadCG
                  testHashedSchedGraph testQuadCG

genVerimTests = do testHashedCodeGen testVerimCG "verimtest"
                   testHashedInterferenceGraph testVerimCG
                   testHashedSchedGraph testVerimCG

genInitMRTests = do testHashedCodeGen testInitMRCG "initmrtest"
                    testHashedInterferenceGraph testInitMRCG
                    testHashedSchedGraph testInitMRCG
-- Quad
testQuad :: forall repr . (CoreISA repr)
  => (repr GPR,repr GPR,repr MR,repr MR,repr GPR,repr GPR)
  -> (repr GPR,repr GPR,repr MR,repr MR,repr GPR,repr GPR)
testQuad (gCnt,gSize,mrIn,mrOut,fifo0,fifo1) =
  let
    -- load
    (v3,mrIn') = moduloVLoad mrIn 0x0 gCnt
    v4 = dfm v3 v3
    -- S0 ^^^^ ;  S1 vvvv
    v5 = dfm v4 v4
    v6 = dfm v5 v5
    v7 = dfm v6 v6
    v8 = dfm v6 v6
    v9 = dfm v7 v7
    v10 = dfm v9 v9
    -- store
    mrOut' = moduloVStore mrOut 0x0 gCnt v10
  in (gCnt,gSize,mrIn',mrOut',fifo0,fifo1)

testQuadCG :: CodeGraph CORE
testQuadCG =
  let
    moduloB = genBlock $ moduloBlock "moduloB"
                                    ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
                                    ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
                                    $ testQuad @(Graph CORE)
  in createCG moduloB

testQuadRun = codeGenAndSimulate ("quad",testQuadCG,1) defaultPenalties0 defaultConstraints Nothing False


testAdd10 :: forall repr . (CoreISA repr)
  => (repr GPR,repr GPR,repr MR,repr MR,repr GPR,repr GPR)
  -> (repr GPR,repr GPR,repr MR,repr MR,repr GPR,repr GPR)
testAdd10 (gCnt,gSize,mrIn,mrOut,fifo0,fifo1) =
  let
    -- load
    (x0,mrIn') = moduloVLoad mrIn 0x0 gCnt
    one = undoubles [1,1]
    x1 = dfa one x0
    x2 = dfa one x1
    x3 = dfa one x2
    x4 = dfa one x3
    x5 = dfa one x4
    x6 = dfa one x5
    x7 = dfa one x6
    x8 = dfa one x7
    x9 = dfa one x8
    x10 = dfa one x9
    x11 = dfa one x10
    x12 = dfa one x11
    -- x13 = dfa one x12
    -- x14 = dfa one x13
    -- x15 = dfa one x14
    -- x16 = dfa one x15
    -- x17 = dfa one x16
    -- x18 = dfa one x17
    -- x19 = dfa one x18
    -- x20 = dfa one x19
    -- x21 = dfa one x20
    -- x22 = dfa one x21
    -- x23 = dfa one x22
    -- x24 = dfa one x23
    -- store
    mrOut' = moduloVStore mrOut 0x0 gCnt x12
  in (gCnt,gSize,mrIn',mrOut',fifo0,fifo1)

testAdd10CG :: CodeGraph CORE
testAdd10CG =
  let
    moduloB = genBlock $ moduloBlock "moduloB"
                                    ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
                                    ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
                                    $ testAdd10 @(Graph CORE)
  in createCG moduloB

add10Wrap :: (String, CodeGraph CORE, Int)
add10Wrap = ("add10",testAdd10CG,1)

testAdd10Sim :: IO (Either (SimError CORE) (HardwareST CORE))
testAdd10Sim = runSimulate add10Wrap testPenalties testConstraints Nothing False

testAdd10CodeGenSim :: IO (Either (SimError CORE) (HardwareST CORE))
testAdd10CodeGenSim = codeGenAndSimulate add10Wrap testPenalties testConstraints Nothing False

testAdd10ModuloDot = testModuloDotGraph add10Wrap testPenalties testConstraints Nothing False

testAdd10SchedDot =  testScheduledDotGraph add10Wrap testPenalties testConstraints Nothing False

testAdd10Liveness :: IO ()
testAdd10Liveness = testLivenessGraph add10Wrap testPenalties testConstraints Nothing False

testAdd10PrintCG = testPrintHashedCG testAdd10CG "add10"

-- Spilling
testSpill :: forall repr . (CoreISA repr)
  => (repr GPR,repr GPR,repr MR,repr MR,repr GPR,repr GPR)
  -> (repr GPR,repr GPR,repr MR,repr MR,repr GPR,repr GPR)
testSpill (gCnt,gSize,mrIn,mrOut,fifo0,fifo1) =
  let
    -- load
    (x0,mrIn') = moduloVLoad mrIn 0x0 gCnt
    one = undoubles [1,1]
    x1 = dfa one x0
    sp = spillVR x1
    x2 = dfa one x1
    x3 = dfa one x2
    x4 = dfa one x3
    x5 = dfa one x4
    x6 = dfa one x5
    x7 = dfa one x6
    x8 = dfa one x7
    x9 = dfa one x8
    x10 = dfa one x9
    x11 = dfa one x10
    x1' = despillVR sp
    x12 = dfa x1' x11
    -- store
    mrOut' = moduloVStore mrOut 0x0 gCnt x12
  in (gCnt,gSize,mrIn',mrOut',fifo0,fifo1)

testSpillCG :: CodeGraph CORE
testSpillCG =
  let
    moduloB = genBlock $ moduloBlock "moduloSpill"
                                    ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
                                    ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
                                    $ testSpill @(Graph CORE)
  in createCG moduloB

spillWrap :: (String, CodeGraph CORE, Int)
spillWrap = ("testSpill",testSpillCG,1)

testSpillPenalties = [topBottomPenalty tScale
                     ,pushLoadConsumes
                     ,spillPenalty
                     ]

testSpillConstraints = [varBounds @CORE
                       ,completionConstraints
                       ,subjectConstraints
                       ,overwriteConstraints
                       ,latConstraints
                       ,gprConstraints
                       -- ,fifoSpillConstraints
                       ,finalInstrConstraint
                       ]

testSpillSim :: IO (Either (SimError CORE) (HardwareST CORE))
testSpillSim = runSimulate spillWrap testSpillPenalties testSpillConstraints Nothing False

testSpillCodeGenSim :: IO (Either (SimError CORE) (HardwareST CORE))
testSpillCodeGenSim = codeGenAndSimulate spillWrap testSpillPenalties testSpillConstraints Nothing False

testSpillModuloDot = testModuloDotGraph spillWrap testSpillPenalties testSpillConstraints Nothing False

-- addChains
testAddChainsSim = runSimulate ("addChains",addChainsCG,4) testPenalties testConstraints Nothing False
testAddChainsLiveness = testLivenessGraph ("addChains",addChainsCG,4) testPenalties testConstraints Nothing False
testAddChainsPrintCG = testPrintHashedCG addChainsCG "addChains"

-- Verim
testVerim :: forall repr s. (CoreISA repr)
  => (repr GPR,repr GPR,repr MR,repr MR,repr GPR,repr GPR)
  -> (repr GPR,repr GPR,repr MR,repr MR,repr GPR,repr GPR)
testVerim (gCnt,gSize,mrIn,mrOut,fifo0,fifo1) =
  let
    -- load
    (v3,mrIn') = moduloVLoad mrIn 0x0 gCnt
    v4 = dfm v3 v3
    -- S0 ^^^^ ;  S1 vvvv
    v5 = dfm v4 v4
    mask = undwrds [0xFFFFFFFFFFFFFFFF, 0]
    v7 = verim 3 0 v5 v4 mask
    -- store
    mrOut' = moduloVStore mrOut 0x0 gCnt v7
  in (gCnt,gSize,mrIn',mrOut',fifo0,fifo1)

testVerimCG :: CodeGraph CORE
testVerimCG =
  let
    moduloB = genBlock $ moduloBlock "moduloB"
                                    ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
                                    ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
                                    $ testVerim @(Graph CORE)
  in createCG moduloB

-- InitMR
testInitMR :: forall repr s. (CoreISA repr)
  => (repr GPR,repr GPR,repr MR,repr MR,repr GPR,repr GPR)
  -> (repr GPR,repr GPR,repr MR,repr MR,repr GPR,repr GPR)
testInitMR (gCnt,gSize,mrIn,mrOut,fifo0,fifo1) =
  let
    -- load
    (v3,mrIn') = moduloVLoad mrIn 0x0 gCnt
    mrTable = initMR 32 "table" [undoubles [1,3],undoubles [1,3]]
    (va,mrTable0) = vld0MR mrTable 0
    (vb,mrTable1) = vld0MR mrTable 16
    v4 = dfm v3 va
    v5 = dfa v4 vb
    -- store
    mrOut' = moduloVStore mrOut 0x0 gCnt v5
  in (gCnt,gSize,mrIn',mrOut',fifo0,fifo1)

testInitMRCG :: CodeGraph CORE
testInitMRCG =
  let
    moduloB = genBlock $ moduloBlock "moduloB"
                                    ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
                                    ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
                                    $ testInitMR @(Graph CORE)
  in createCG moduloB

--------------------------------------------------------------------------------------------
-- Simulator
--------------------------------------------------------------------------------------------
defaultSolver = Ipopt
dfScale (n,dfNode) = 10000
defaultPenalties0 = [topBottomPenalty @CORE dfScale,pushLoadConsumes,spillPenalty]
defaultConstraints = [varBounds @CORE,completionConstraints,subjectConstraints,overwriteConstraints
                 ,latConstraints
                 ,gprConstraints
                 ,spillConstraints
                 ,finalInstrConstraint
                 ]
defStages = 2
defaultFixedRegs = [("size","3"),("mrIn","2"),("mrOut","1")]

codeGenAndSimulate :: (String, CodeGraph CORE, Int)
                -> [HashedPenalty CORE]
                -> [HashedConstraint CORE]
                -> Maybe StdGen
                -> Bool
                -> IO (Either (SimError CORE) (HardwareST CORE))
codeGenAndSimulate wrappedCG penalties constraints stdgen reuse = do
  (cg,sched,regMap,spillMap,schedGraph,ii,stdgen') <- scheduleAndRegAllocate moduloCG unroll coreMetaData penalties constraints stdgen reuse defaultSolver defStages defaultFixedRegs
  let (code,tables) = codeGenCG cg (regMap,spillMap,schedGraph) coreMetaData
      allCode = moduloZHeader (name,stdgen') ++ code ++ moduloZReturn ++ tables ++ moduloZEnd
  BS.writeFile ("asm/"++name++".s"  )$ BS.unlines allCode
  initSimulator ("asm/"++name++".dbxin" ) schedGraph cg (regMap,spillMap) [mrIn, mrOut] (initGPR, [])
  where
    (name,moduloCG0,unroll) = wrappedCG
    moduloCG = cgApplyRewrites moduloCG0
    mrIn  = ("mrIn", "2", map undoubles $ zipWith (\x y -> [x,y]) [0,2..6] [1,3..7])
    -- mrIn  = ("mrIn", "2", map undoubles $ zipWith (\x y -> [x,y]) [0,2..30] [1,3..31])
    mrOut = ("mrOut", "1", map undoubles2 $ replicate 8 99.9)
    -- mrOut = ("mrOut", "1", map undoubles2 $ replicate 32 99.9)
    initGPR = [("3", unintegerG (8*8))]

runSimulate :: (String, CodeGraph CORE, Int)
                -> [HashedPenalty CORE]
                -> [HashedConstraint CORE]
                -> Maybe StdGen
                -> Bool
                -> IO (Either (SimError CORE) (HardwareST CORE))
runSimulate wrappedCG penalties constraints stdgen reuse = do
  (cg,sched,regMap,spillMap,schedGraph,ii,stdgen') <- scheduleAndRegAllocate moduloCG unroll coreMetaData penalties constraints stdgen reuse defaultSolver defStages defaultFixedRegs
  -- let (code,tables) = codeGenCG cg (regMap,spillMap,schedGraph) coreMetaData
  --     allCode = moduloZHeader (name,stdgen') ++ code ++ moduloZReturn ++ tables ++ moduloZEnd
  -- BS.writeFile ("asm/"++name++".s"  )$ BS.unlines allCode
  initSimulator ("asm/"++name++".dbxin" ) schedGraph cg (regMap,spillMap) [mrIn, mrOut] (initGPR, [])
  where
    (name,moduloCG0,unroll) = wrappedCG
    moduloCG = cgApplyRewrites moduloCG0
    -- mrIn  = ("mrIn", "2", map undwrds $ zipWith (\x y -> [x,y]) [0,2..6] [1,3..7])
    mrIn  = ("mrIn", "2", map undoubles $ zipWith (\x y -> [x,y]) [0,2..6] [1,3..7])
    mrOut = ("mrOut", "1", map undoubles2 $ replicate 8 0)
    -- mrOut = ("mrOut", "1", map undoubles2 $ replicate 32 99.9)
    initGPR = [("3", unintegerG (8*8))]

testScheduledDotGraph wrappedCG penalties constraints stdgen reuse = do
  (cg,sched,regMap,spillMap,schedGraph,ii,stdgen') <- scheduleAndRegAllocate moduloCG unroll coreMetaData penalties constraints stdgen reuse defaultSolver defStages defaultFixedRegs
  dotStringScheduleFormatted cg (buildScheduledGraph sched cg) sched "./dot"
  where
    (name,moduloCG0,unroll) = wrappedCG
    moduloCG = cgApplyRewrites moduloCG0

testSimInitMR :: IO ()
testSimInitMR = do
  (cg,sched,regMap,spillMap,schedGraph,ii,stdgen') <- scheduleAndRegAllocate testInitMRCG 1 coreMetaData testPenalties testConstraints Nothing False defaultSolver defStages defaultFixedRegs
  -- dotScheduleCompilation cg schedGraph "."
  print regMap
  output <- initSimulator
    "asm/testSimInitMR.dbxin"
    schedGraph
    cg
    (regMap,spillMap)
    initMRs
    (initGPRs, [])
  print output
  where
    initGPRs = [("3", unintegerG 4), ("4", unintegerG 4)]
    initMRs =
      [("mrIn", "2", [undoubles2 1])
      ,("mrOut", "1", [undoubles2 0])
      ]

--------------------------------------------------------------------------------------------
-- Test Utitlities
--------------------------------------------------------------------------------------------
tScale (n,dfNode) = 10000
testPenalties = [topBottomPenalty tScale,pushLoadConsumes]
testConstraints = [varBounds,completionConstraints,subjectConstraints,overwriteConstraints
                  ,latConstraints ,gprConstraints,finalInstrConstraint ]

testPrintHashedCG :: CodeGraph CORE -> String -> IO ()
testPrintHashedCG codeGraph name = do
  (cg,sched,regMap,spillMap,schedGraph,ii,stdgen') <- scheduleAndRegAllocate moduloCG 1 coreMetaData testPenalties testConstraints Nothing False defaultSolver defStages defaultFixedRegs
  Prelude.putStrLn $ unlines $ prettyCodeGraph cg
  where
    moduloCG = cgApplyRewrites codeGraph

testHashedCodeGen :: CodeGraph CORE -> String -> IO ()
testHashedCodeGen codeGraph name = do
  (cg,sched,regMap,spillMap,schedGraph,ii,stdgen') <- scheduleAndRegAllocate codeGraph 1 coreMetaData testPenalties testConstraints Nothing False defaultSolver defStages defaultFixedRegs
  let (code,tables) = codeGenCG cg (regMap,spillMap,schedGraph) coreMetaData
      allCode = moduloZHeader (name,stdgen') ++ code ++ moduloZReturn ++ tables ++ moduloZEnd
  -- BS.putStrLn $ BS.unlines allCode
  BS.writeFile ("asm/"++name++".s") $ BS.unlines allCode

testHashedInterferenceGraph :: CodeGraph CORE -> IO ()
testHashedInterferenceGraph codeGraph = do
  (cg,sched,regMap,spillMap,schedGraph,ii,stdgen') <- scheduleAndRegAllocate codeGraph 1 coreMetaData testPenalties testConstraints Nothing False defaultSolver defStages defaultFixedRegs
  dotCompilationInterference (case genInterferenceGraph cg schedGraph of
                                (_,_,g) -> g) regMap "./dot"

testHashedSchedGraph :: CodeGraph CORE -> IO ()
testHashedSchedGraph codeGraph = do
  (cg,sched,regMap,spillMap,schedGraph,ii,stdgen') <- scheduleAndRegAllocate codeGraph 1 coreMetaData testPenalties testConstraints Nothing False defaultSolver defStages defaultFixedRegs
  dotStringScheduleFormatted cg (buildScheduledGraph sched cg) sched "./dot"
  dotCompilation cg

testLiveness :: CodeGraph CORE -> IO ()
testLiveness codeGraph = do
  (cg,sched,regMap,spillMap,schedGraph,ii,stdgen') <- scheduleAndRegAllocate codeGraph 1 coreMetaData testPenalties testConstraints Nothing False defaultSolver defStages defaultFixedRegs
  let cMap = registerCoalesceMap cg
  let schedGraph = buildScheduledGraph sched cg
  Prelude.putStrLn $ show $ livenessSets schedGraph cMap
  Prelude.putStrLn $ show $ registerCoalesceMap cg

testLivenessGraph :: (String, CodeGraph CORE, Int)
                      -> [HashedPenalty CORE]
                      -> [HashedConstraint CORE]
                      -> Maybe StdGen
                      -> Bool
                      -> IO ()
testLivenessGraph wrappedCG penalites constraints stdgen reuse = do
  (cg,sched,ii,_,_) <- genHashedSchedule moduloCG unroll coreMetaData penalites constraints stdgen reuse defaultSolver defStages
  let scheduledGraph = buildScheduledGraph sched cg
      allNodes = topsort scheduledGraph
      (coalesceMap,liveSets,interGraph) = genInterferenceGraph cg scheduledGraph
      scheduledGraph' = insertRegPressureSchedGraph scheduledGraph interGraph liveSets
  printLiveSets allNodes liveSets
  Prelude.putStrLn "generating dot graph"
  dotStringScheduleFormatted cg scheduledGraph' sched "./dot"
  Prelude.putStrLn "done"
  -- dotCompilationInterference (snd $ genInterferenceGraph cg sched) (fromRight Map.empty $ regAllocateCG sched cg [("size","3"),("mrIn","2"),("mrOut","1")]) "./dot"
  where
    (name,moduloCG0,unroll) = wrappedCG
    moduloCG = cgApplyRewrites moduloCG0
    printLiveSets [] _ = return ()
    printLiveSets (n:ns) liveSets =
      do Prelude.putStrLn $ "Node: " ++ show n
         Prelude.putStrLn $ "    LivenessIn: " ++ (show $ fromJust $ IntMap.lookup n $ livenessIn liveSets)
         Prelude.putStrLn $ "    LivenessOut: " ++ (show $ fromJust $ IntMap.lookup n $ livenessOut liveSets)
         printLiveSets ns liveSets

testModuloDotGraph :: (String, CodeGraph CORE, Int)
                       -> [HashedPenalty CORE]
                       -> [HashedConstraint CORE]
                       -> Maybe StdGen
                       -> Bool
                       -> IO ()
testModuloDotGraph wrappedCG penalites constraints stdgen reuse = do
  (cg,sched,ii,_,_) <- genHashedSchedule moduloCG unroll coreMetaData penalites constraints stdgen reuse defaultSolver defStages
  dotCompilation cg
  where
    (name,moduloCG0,unroll) = wrappedCG
    moduloCG = cgApplyRewrites moduloCG0

moduloZHeader :: (String,StdGen) -> [BS.ByteString]
moduloZHeader (name,stdgen) = [
     "* " <> (BS.pack $ show stdgen)
    ,(BS.pack $ map toUpper name) <> "   CELQPRLG BASEREG=NONE,DSASIZE=MYDSASZ,LEAF=YES"
    ,(BS.pack $ map toUpper name) <> "   ALIAS C'"<>BS.pack name<>"'"
    ,"    USING MYDSA,4         * establish addressibility to DSA"
    ,"    VSTM  V16,V23,VRSAVE   * save caller's V16 - V23 (non_volatile)"
    ,"    STD   8,FRSAVE        * save caller's FPR 8 - 15 (non_volatile)"
    ,"    STD   9,FRSAVE+8      * only if you have clobbered F8 - F15 or"
    ,"    STD   10,FRSAVE+16    * V8 - V15"
    ,"    STD   11,FRSAVE+24"
    ,"    STD   12,FRSAVE+32"
    ,"    STD   13,FRSAVE+40"
    ,"    STD   14,FRSAVE+48"
    ,"    STD   15,FRSAVE+56"
    ,"    lgfi  R0,64  * R0 hardcoded as counter"
    ,"    llgf  R3,0(,R3) * load size into R3"
    ,"    sllg  R3,R3,3(R0)  * 8 bytes per double"
    ]

moduloZReturn = [
     "RETURN DS 0H"
    ,"    VLM  V16,V23,VRSAVE"
    ,"    LD   8,FRSAVE"
    ,"    LD   9,FRSAVE+8"
    ,"    LD   10,FRSAVE+16"
    ,"    LD   11,FRSAVE+24"
    ,"    LD   12,FRSAVE+32"
    ,"    LD   13,FRSAVE+40"
    ,"    LD   14,FRSAVE+48"
    ,"    LD   15,FRSAVE+56"
    ,"    CELQEPLG"
                ]
moduloZEnd = [
     "MYDSA    DSECT"
    ,"VRSAVE   DS    8L"
    ,"FRSAVE   DS    8D              insert automatic data here as needed"
    ,"FIFOS    DS    16L"
    ,"MYDSASZ  EQU   *-MYDSA"
    ,"R0       EQU   0"
    ,"R1       EQU   1"
    ,"R2       EQU   2"
    ,"R3       EQU   3"
    ,"R4       EQU   4"
    ,"R5       EQU   5"
    ,"R6       EQU   6"
    ,"R7       EQU   7"
    ,"R8       EQU   8"
    ,"R9       EQU   9"
    ,"R10       EQU   10"
    ,"R11       EQU   11"
    ,"R12       EQU   12"
    ,"R13       EQU   13"
    ,"R14       EQU   14"
    ,"R15       EQU   15"
    ,"V0       EQU   0"
    ,"V1       EQU   1"
    ,"V2       EQU   2"
    ,"V3       EQU   3"
    ,"V4       EQU   4"
    ,"V5       EQU   5"
    ,"V6       EQU   6"
    ,"V7       EQU   7"
    ,"V8       EQU   8"
    ,"V9       EQU   9"
    ,"V10       EQU   10"
    ,"V11       EQU   11"
    ,"V12       EQU   12"
    ,"V13       EQU   13"
    ,"V14       EQU   14"
    ,"V15       EQU   15"
    ,"V16       EQU   16"
    ,"V17       EQU   17"
    ,"V18       EQU   18"
    ,"V19       EQU   19"
    ,"V20       EQU   20"
    ,"V21       EQU   21"
    ,"V22       EQU   22"
    ,"V23       EQU   23"
    ,"V24       EQU   24"
    ,"V25       EQU   25"
    ,"V26       EQU   26"
    ,"V27       EQU   27"
    ,"V28       EQU   28"
    ,"V29       EQU   29"
    ,"V30       EQU   30"
    ,"V31       EQU   31"
    ,"    END"
  ]

{-

Modulo Scheudling

Assume we have 2 stages

S0
S1

we have gSize the size of the loop
use a local variable count

// numStages = 2
// basic block 0
count := 0
if gSize > 0 then
  // basic block 1
  S0[0]
  while count < gSize - (numStages - 1) do
    // basic block 2
    S0[count+1] ; S1[count]
    count++
  // basic block 3
  S1[count]
else
  {}
return

Assume we have 2 stages

S0
S1
S3

we have gSize the size of the loop
use a local variable count

// basic block 0
count := 0
if gSize > 0 then
  // basic block 1
  S0[0]
  if gSize > 1 then
    // basic block 2
    S1[0] ; S0[1]
    while count < gSize - (numStages - 1) do
      // basic block 3
      S0[count+2] ; S1[count+1] ; S2[count]
    // basic block 4
    S1[count+1] ; S2[count]
    count++
  else
    // basic block 5
    S2[count+1]
else
  {}
return

-}

-- II = number of floating point operations / 2
