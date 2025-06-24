module Coconut.Utils.CGUtils where

import Coconut.BaseTypes

import Data.Graph.Inductive.Graph

instrCount :: CodeGraph h -> Int
instrCount cg = sum $ map (instrCountDFG . dataFlowGraph . snd) (cgDataFlows cg)
  where
    instrCountDFG = length . filter isInstrNode . labNodes
    isInstrNode (_, InstructionNode _) = True
    isInstrNode _                      = False
