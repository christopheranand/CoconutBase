-- |
-- Module      :  Coconut.Simulator
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a functionality for simulating coconut code

{-# LANGUAGE OverloadedStrings #-}
module Coconut.Simulator where
-- TODO rewrite simulator

import Data.Maybe
import Data.Word
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Except

import Coconut.BaseTypes
import Coconut.Utils.ArbFloat (word642Dbl)

import System.IO
import GHC.Int (Int64)

import Debug.Trace

-- Top level simulation function
initSimulator :: Hardware h => FilePath
                            -> ScheduledGraph h
                            -> CodeGraph h
                            -> (RegMap h,SpillMap)
                            -> [(String, ByteString, [Interp VR])]
                            -> ([(ByteString, Interp GPR)], [(ByteString, Interp VR)])
                            -> IO (Either (SimError h) (HardwareST h))
initSimulator dbxFile schedGraph cg (regMap,spillMap) inputMRs inputs =
  runExceptT $ fmap snd $ runStateT (runSimState (runSimulator dbxFile schedGraph cg (regMap,spillMap))) (initHardwareST (regMap,spillMap) inputMRs inputs)

-- | Wraps the scheduled graph simulator and determines/passes the initial node to it
runSimulator :: Hardware h => FilePath -> ScheduledGraph h -> CodeGraph h -> (RegMap h,SpillMap) -> SimState h ()
runSimulator dbxFile schedGraph cg (regMap,spillMap) =
  let initNode = head $ topsort schedGraph
  in do
    fileHandle <- liftIO $ openFile dbxFile WriteMode
    liftIO $ debugPrintCgLabels cg
    simSchedGraph fileHandle schedGraph cg (regMap,spillMap) initNode

-- | Traverse the scheduled graph, printing out states before and after instruction application
simSchedGraph
  :: Hardware h => Handle -> ScheduledGraph h -> CodeGraph h -> (RegMap h,SpillMap) -> Node -> SimState h ()
simSchedGraph handle schedGraph cg (regMap,spillMap) node = do
  liftIO $ putStrLn $ "simSchedGraph at node " ++ show node
  prevState <- get
  -- liftIO $ print $ showHardwareST prevState
  runInstruction handle schedGraph cg (regMap,spillMap) node
  curState <- get
  -- liftIO $ print $ showHardwareST curState
  liftIO $ putStrLn ""
  case out schedGraph node of
    [] -> liftIO $ hClose handle
    [(_,nextNode,SECompose)] -> simSchedGraph handle schedGraph cg (regMap,spillMap) nextNode -- Linked next node, no branch
    [(_,branchNENode,SEBranchNE), (_,branchEQNode,SEBranchEQ)] -> -- If we encounter a branch in the graph
      simSchedGraph handle schedGraph cg (regMap,spillMap) $
        if getBranchCond curState then branchEQNode else branchNENode
    [(_,branchEQNode,SEBranchEQ), (_,branchNENode,SEBranchNE)] -> -- If we encounter a branch in the graph
      simSchedGraph handle schedGraph cg (regMap,spillMap) $
        if getBranchCond curState then branchEQNode else branchNENode
    lEdges -> error $ "simSchedGraph encountered bad out edge pattern: " ++ show lEdges
  -- case suc schedGraph node of
  --   [] -> liftIO $ hClose handle
  --   [nextNode] -> simSchedGraph handle schedGraph cg regMap nextNode -- Linked next node, no branch
  --   [branchNENode, branchEQNode] -> -- If we encounter a branch in the graph
  --     simSchedGraph handle schedGraph cg regMap $
  --       if getBranchCond curState then branchEQNode else branchNENode

prettyPrintMRTable :: (ByteString,Interp MR) -> IO ()
prettyPrintMRTable (label,mrTable) =
  let
    currentMemory :: ByteString
    currentMemory = current $ runInterpMR mrTable

    bytes2Dwrd :: [Word8] -> Word64
    bytes2Dwrd xs = sum (zipWith (\ x y -> fromIntegral y * 2^(8*x)) [7,6..0] xs)

    chunks :: Int -> [a] -> [[a]]
    chunks _ [] = []
    chunks k xs = let (ys, zs) = splitAt k xs in ys : chunks k zs

    memAsDWrds :: [(Integer,Double)]
    memAsDWrds = zip [0,8..] $ map (word642Dbl . bytes2Dwrd ) $ chunks 8 $ BS.unpack $ currentMemory
  in do putStrLn  $ "Input MR: " ++ BSC.unpack label
        mapM_ (putStrLn . show) memAsDWrds

debugPrintCgLabels :: Hardware h => CodeGraph h -> IO ()
debugPrintCgLabels =
  print . Map.map (\s -> take 20 s ++ "TRUNCATED") . cgDebugLabels
