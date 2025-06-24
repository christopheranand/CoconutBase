{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module SimSpec where

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

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Either (fromRight)
import Data.IntMap (IntMap)
import GHC.TypeNats (KnownNat)
import Data.Maybe (fromJust)
import qualified Data.List as List
import System.Process
import Coconut.CodeGen
import Data.Bifunctor
import qualified Data.Graph.Inductive.Graph as FGL

testAdd :: forall repr . CoreISA repr => repr VR -> repr VR
testAdd w =
  let x = undoubles2 2
      y = undoubles2 3
  in dfm w $ dfa x y

testCG :: CodeGraph CORE
testCG = createCG (genBlock (basicBlock "addBlock" ["v4"] ["v6"] (testAdd @(Graph CORE))))

testSchedGraph :: ScheduledGraph CORE
testSchedGraph = buildScheduledGraph (defaultSchedule testCG) testCG

testRegAllocateCG :: (RegMap CORE,SpillMap,ScheduledGraph CORE)
testRegAllocateCG =
  let schedule = defaultSchedule testCG
  in fromRight (error "failed regAllocateCG") $ regAllocateCG schedule testCG
     [("size", "3"), ("mrIn", "2"), ("mrOut", "1")]

testCodeGen :: ([ByteString], [ByteString])
testCodeGen =
  let
    schedule = defaultSchedule testCG
    (regMap,spillMap,schedGraph) =
      case regAllocateCG schedule testCG [("size","5"),("mrIn","2"),("mrOut","1")] of
        Right r -> r
        Left ns -> error $ "failed regAllocateCG with leftover nodes: " ++ show ns
  in codeGenCG testCG (regMap,spillMap,schedGraph) coreMetaData

printTestCodeGen :: IO ()
printTestCodeGen = BS.putStrLn $ joinTuple $ bimap BS.unlines BS.unlines testCodeGen
  where joinTuple (a, b) = a <> b

testSim :: IO (Either (SimError CORE) (HardwareST CORE))
testSim = initSimulator "asm/testSim" testSchedGraph testCG (case testRegAllocateCG of
                                                               (r,s,_) -> (r,s)) [] ([], initVR)
  where
    initVR = [("4", undoubles2 10)]
