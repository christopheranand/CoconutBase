-- |
-- Module      :  test.PowerInterpTests
-- Copyright   :  (c) OCA 2023
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module contains simple tests for PowerISA Interp instructions

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module PowerInterpTests where

import Coconut.BaseTypes
import Coconut.Power.PowerISA
import Coconut.Power.PowerInterp

-- Test add
testAdd :: forall repr . PowerISA repr => (repr GPR,repr GPR) -> repr GPR
testAdd (g0,g1) =
  let
    g2 = add g0 g1
  in g2

interpTestAdd :: Bool
interpTestAdd = runInterpGPR (testAdd @Interp (unintegerG 0,unintegerG 1)) == 1
