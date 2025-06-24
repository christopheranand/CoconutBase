-- |
-- Module      :  test.Simple
-- Copyright   :  (c) OCA 2023
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module contains simple tests developed by summer 2023 team

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Simple where


import Coconut.BaseTypes
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
import Data.ByteString
import Data.ByteString.Char8 as BS

testA :: forall repr . CoreISA repr => (repr GPR,repr GPR) -> repr GPR
testA (g4,g1) =
  let
    g6 = addG g4 g1
    g7 = addG g6 g1
  in g7


interpTestA :: Interp GPR
interpTestA = testA @Interp (unintegerG 0,unintegerG 1)

testB :: forall repr . CoreISA repr => (repr VR,repr VR) -> repr VR
testB (v0,v1) =
  let
    v2 = dfa v0 v1
    v3 = dfa v2 v1
  in v3

interpTestB :: [Double]
interpTestB = doubles $ testB @Interp (undoubles [0,1],undoubles [1,2])


-- * Code Graph Construction
testB_CG :: CodeGraph CORE
testB_CG = createCG
         $ genBlock
         $ basicBlock "testB" ["v0","v1"] ["v3"] (testB @(Graph CORE))

dotGenTestB = dotCompilation testB_CG

-- * Simulation
simulateBasicCG :: Hardware h =>
  CodeGraph h
  -> [(String, ByteString)]
  -> [(String, ByteString, [Interp VR])]
  -> ([(ByteString, Interp GPR)],
      [(ByteString, Interp VR)])
  -> IO (Either (SimError h) (HardwareST h))
simulateBasicCG cg fixedRegs initMRs (initGPRs,initVRs) =
  let
    sched = defaultSchedule cg
    eReg = regAllocateCG sched cg fixedRegs
    (regMap,spillMap,schedGraph) =
      case eReg of
        Right s -> s
        Left (failedNodes,_) -> error $ "failed to reg alloc " ++ show failedNodes
  in initSimulator "asm/test.dbxin" schedGraph cg (regMap,spillMap) initMRs (initGPRs,initVRs)



testBSimulate :: IO (Either (SimError CORE) (HardwareST CORE))
testBSimulate =
  let
    fixedRegs = [("v0","1"),("v1","2")]
    initMRs = []
    initGPRs = []
    initVRs = [("1",undoubles @Interp [1,1])
              ,("2",undoubles @Interp [0,0])]
  in simulateBasicCG testB_CG fixedRegs initMRs (initGPRs,initVRs)


-- * CodeGen
codeGenBasicCG :: CodeGraph CORE -> [(String, ByteString)] -> IO ()
codeGenBasicCG cg fixedRegs =
  let
    sched = defaultSchedule cg
    eReg = regAllocateCG sched cg fixedRegs
    (regMap,spillMap,schedGraph) =
      case eReg of
        Right s -> s
        Left (failedNodes,_) -> error $ "failed to reg alloc: " ++ show failedNodes
    (code,tables) = codeGenCG cg (regMap,spillMap,schedGraph) coreMetaData
  in BS.writeFile "asm/test.s" $ BS.unlines $ code ++ tables
