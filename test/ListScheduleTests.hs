-- Module      :  test.ListScheduleTests
-- Copyright   :  (c) OCA 2021
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module is set up to experiment with list scheduling algorithms


{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module ListScheduleTests where


import Coconut.BaseTypes
import Coconut.ListSchedule
import Coconut.Utils.CGWrappers
import Coconut.Graph.DataFlow
import Coconut.Graph.ControlFlow
import Coconut.Graph.CodeGraph
import Coconut.Core.CoreISA
import Coconut.Core.ControlISA (compose,branch,doWhile)
import Coconut.Core.CoreHardware (CORE)
import Coconut.Core.MetaData
import Coconut.CodeGen
import Coconut.RegisterAllocator
import Coconut.Schedule
import Coconut.Graph.Dot
import Coconut.Simulator
import Coconut.HashedSchedule
import Coconut.Rewrite
import Data.ByteString
import Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BS

import Data.Graph.Inductive.Graph hiding (Graph)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS (topsort)

import qualified Data.Map as Map
import qualified Data.List as List

testZ :: forall repr . CoreISA repr => repr GPR -> repr GPR
testZ g0 =
  let
    g1 = addG g0 g0
    g2 = addG g1 g1
    g3 = addG g2 g2
    g4 = addG g3 g3
    g5 = addG g4 g4
    g6 = addG g5 g5
    g7 = addG g6 g6
    g8 = addG g7 g7
    g9 = addG g8 g8
    g10 = addG g9 g9
    g11 = addG g10 g10
    g12 = addG g11 g11
    g13 = addG g12 g12
    g14 = addG g13 g13
    g15 = addG g14 (unintegerG 1) -- g14 -- (unintegerG 1) WHY NO WORK?
    g16 = addG g15 (unintegerG 1)
  in
    g16
interpTestZ :: Interp GPR
interpTestZ = testZ @Interp (unintegerG 2)

testVerim :: forall repr s. (CoreISA repr) =>
  repr VR -> repr VR
testVerim v3 =
  let
    -- load
    v4 = dfm v3 v3
    -- S0 ^^^^ ;  S1 vvvv
    v5 = dfm v4 v4
    mask = undwrds [0xFFFFFFFFFFFFFFFF, 0]
    v7 = verim 3 0 v5 v4 mask
    -- store
    v8 = dfa (undoubles2 1) v5
  in dfa v7 v8

testVerimCG :: CodeGraph CORE
testVerimCG = createCG $ genBlock $
  basicBlock "testVerim" ["v8"] ["v9"] (testVerim @(Graph CORE))

testZ_CG :: CodeGraph CORE
testZ_CG = createCG
         $ genBlock
         $ basicBlock "testZ" ["g8"] ["g9"] (testZ @(Graph CORE))

testScc :: forall repr. CoreISA repr => (repr GPR, repr GPR) -> repr GPR
testScc (g0, g1) =
  let
    mul0 = mulldG g0 (unintegerG 2)
    add0 = addG g1 (unintegerG 5)
    and0 = andG (unintegerG 3) (unintegerG 4)
  in
    addG and0 (addG mul0 add0)

testSccCG :: CodeGraph CORE
testSccCG = createCG
             $ genBlock
             $ basicBlock "testSccCG" ["g8", "g9"] ["g10"] (testScc @(Graph CORE))

dotGenTestZ = dotCompilation testZ_CG

-- TODO: Allison test `branch` and `doWhile`

ii2o :: [(ByteString, Interp GPR)]
ii2o = [("8", unintegerG 11), ("9", unintegerG 12), ("10", unintegerG 13)]

i2o :: [(ByteString, Interp GPR)]
i2o = [("8", unintegerG 2), ("9", unintegerG 10)]

codeGenAndSimulateWithListSchedule :: CodeGraph CORE
  -> String
  -> [(ByteString, Interp GPR)]
  -> IO (Either (SimError CORE) (HardwareST CORE))
codeGenAndSimulateWithListSchedule cg name initGPRs = do
  let cg' = cgApplyRewrites cg
      sched = listSchedule cg' coreMetaData
      regMap0 = regAllocateLinearScanCG sched cg [("g8", "8"), ("g9", "9"), ("g10", "10")]
        -- regAllocateCG sched cg' [("g8", "8"), ("g9", "9"), ("g10", "10")]
      ((code,tables),(regMap,spillMap',schedGraph)) =
        case regMap0 of
          Right (rMap,spillMap,sGraph) -> (codeGenCG cg' (rMap,spillMap,sGraph) coreMetaData,(rMap,spillMap,sGraph))
          Left ns -> error $ "failed to register allocate with leftever nodes: " ++ show ns
      allCode = code
  BS.writeFile ("asm/"++name++".s"  )$ BS.unlines allCode
  dotScheduleCompilation cg' schedGraph sched "dot"
  print regMap
  initSimulator ("asm/"++name++".dbxin" ) schedGraph cg' (regMap,spillMap') [] (initGPRs, initVRs)
  where
    initVRs = []

testCG :: forall h. Hardware h => CodeGraph h
testCG =
  createCG $ genBlock $ basicBlock "basicTestCG"
    inputLabels outputLabels (testZ @(Graph h))
  where
    inputLabels = ["inputG8"]
    outputLabels = ["outputG9"]

