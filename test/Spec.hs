-- |
-- Module      :  test.Main
-- Copyright   :  (c) OCA 2021
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module is the main module for the Coconut test-suite

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

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

import AddChains
import HashedScheduleTests

-- cg = ("add10",testAdd10CG,1)
cg = ("addChains",addChainsCG,4)

main = do
  testLivenessGraph cg testPenalties testConstraints Nothing False
  -- runSimulate cg testPenalties testConstraints Nothing False
  -- testModuloDotGraph cg testPenalties testConstraints Nothing False
  -- testHashedSchedGraph $ case cg of { (_,cg',_) -> cg' }
  -- testPrintHashedCG (case cg of { (_,cg',_) -> cg' }) "add10"
  putStrLn "Complete!"
