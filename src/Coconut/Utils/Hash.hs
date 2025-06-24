-- |
-- Module      :  Coconut.BaseTypes
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module is used for hashing nodes in codegraph construction, to speed
-- up comparison in common subexpression elimination. The hash interface
-- and algorithm is similar to what is used in HashedExpression


{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, LambdaCase #-}

module Coconut.Utils.Hash where

import qualified Data.IntMap.Strict as IM

import Data.Graph.Inductive
import Data.Char (ord)
import Data.List (intercalate)
import Coconut.BaseTypes
import qualified Data.Hashable as Hash

import Debug.Trace

-- | Check the outcome of a generated hash value
data HashOutcome
  = -- | clashes with an old hash
    IsClash
  | -- | new hash value
    IsOk Int
  deriving (Eq, Show, Ord)

-- | A collision occurs when two inputs hash to the same number and
--   their node labels and node inputs are exactly the same
type CheckCollision h = ([Node],DFNode h) -> Int -> HashOutcome

-- | hardcoded modulos used in hash function (i.e 'hashString')
modulo :: Int
modulo = 253931039382791

-- | hardcoded radix used in hash function (i.e 'hashString')
radix :: Int
radix = 83

-- | Hash string
-- s[0] + s[1]⋅p + s[2]⋅p^2 + ... + s[n−1]⋅p^n−1 mod m
hashString :: String -> Int
hashString (x : xs) = ((ord x - ord '0') + radix * hashString xs) `mod` modulo
hashString [] = 0

-- | Offset the hash by number of m
offsetHash :: Int -> Int -> Int
offsetHash offset h =
  Hash.hash offset
  `Hash.hashWithSalt` h

-- | Any string used as a separator between node ids (which are numbers)
separator :: String
separator = "a"

-- | Compute a hash value for a given node, instruction and inputs, and number of rehash
hash :: (Show (RegType h)) => [Node] -> DFNode h -> Int
hash inps dfNode =
  let hashFunc n h = offsetHash n (
        Hash.hash inps
        `Hash.hashWithSalt` h)
  in case dfNode of
    InstructionNode (Instruction imms name _) ->
      -- trace name $ traceShow imms $ traceShowId $
      case imms of
        _ -> hashFunc 0 (Hash.hash imms `Hash.hashWithSalt` Hash.hash name)

    InstructionNode (Move name _) ->
      hashFunc 0 (Hash.hash name)
    InstructionNode (Spill name) ->
      hashFunc 0 (Hash.hash "spill" `Hash.hashWithSalt` Hash.hash name)
    InstructionNode (Despill name) ->
      hashFunc 0 (Hash.hash "despill" `Hash.hashWithSalt` Hash.hash name)
    InstructionNode (InitMR lbl imm) ->
      hashFunc 1 (Hash.hash lbl)
    ResourceNode n ->
      hashFunc 2 (Hash.hash (show n))
    BranchNode instr imms _ ->
      hashFunc 3 (Hash.hash imms `Hash.hashWithSalt` Hash.hash instr)
    _ -> error $ "no hash for node " ++ show dfNode
{-
hash inps dfNode rehashNum =
  let hashString' s =
        hashString $
           (intercalate separator . map show $ inps)
        ++ separator
        ++ s
        ++ concat (replicate rehashNum "x")
  in case dfNode of
    InstructionNode (Instruction imms name _) ->
      offsetHash 0 . hashString' $
        (intercalate separator . map show $ imms)
      ++ separator
      ++ show name
    InstructionNode (InitMR lbl imm) ->
      offsetHash 1 . hashString' $ lbl
    ResourceNode n ->
      offsetHash 2 . hashString' $ show n
    BranchNode instr imms _ ->
      offsetHash 3 . hashString' $
        (intercalate separator . map show $ imms)
      ++ separator
      ++ show instr
    _ -> error $ "no hash for node " ++ show dfNode
-}

equalsNode :: (Eq (RegType h)) => DFNode h -> DFNode h -> Bool
equalsNode (ResourceNode a) (ResourceNode b) = a == b
equalsNode (InstructionNode (Instruction imm0 name0 _)) (InstructionNode (Instruction imm1 name1 _)) =
  imm0 == imm1 && name0 == name1

equalsNode (InstructionNode (Move name0 _)) (InstructionNode (Move name1 _)) =
  name0 == name1
equalsNode (InstructionNode (InitMR lbl0 _)) (InstructionNode (InitMR lbl1 _)) =
  lbl0 == lbl1
equalsNode (BranchNode name0 imms0 _) (BranchNode name1 imms1 _) =
    name0 == name1 && imms0 == imms1

{-
-- | IsOk if doesn't collide with the provided expression map
--   IsClash otherwise
checkCollisionMap :: Hardware h => CGNodeMap h -> CheckCollision h
checkCollisionMap mp (inps,node) hashKey =
  case IM.lookup hashKey mp of
    Nothing -> IsOk hashKey
    Just ((node',inps'),_,_) ->
      if equalsNode node node' && inps == inps'
        then IsClash
        else IsOk hashKey

-- | IsOk if doesn't collide with any of provided expression map
--   IsClash otherwise
checkCollisionMaps :: Hardware h => [CGNodeMap h] -> CheckCollision h
checkCollisionMaps [] _ hashKey = IsOk hashKey
checkCollisionMaps (mp : mps) node hashKey =
  case checkCollisionMap mp node hashKey of
    IsClash -> IsClash
    IsOk _ -> checkCollisionMaps mps node hashKey


-- | Hash a node with a collision checking function
hashNode :: Hardware h => CheckCollision h -> [Node] -> DFNode h -> Int
hashNode checkCollision inps node =
  case dropWhile (== IsClash) . map (checkCollision (inps, node) . hash inps node) $ [0 .. 1000] of
    (IsOk h : _) -> h
    _ -> error "hashNode everything clashed!"
-}

-- | Given a hashmap and hash inputs, compute if the hash exists, taking
--   into account hash collisions
hashExists :: Hardware h => [Node] -> DFNode h -> CGNodeMap h -> Maybe (Node, [Node])
hashExists inputs node mp =
  case IM.lookup (hash inputs node) mp of
    Nothing -> Nothing
    Just vals -> sameInstructionAndInputs vals

  where
    sameInstructionAndInputs [] = Nothing
    sameInstructionAndInputs (((dfNode,inputs'),nodeID,outputs):next) =
      if equalsNode node dfNode -- && inputs == inputs'
      then Just (nodeID, outputs)
      else sameInstructionAndInputs next

insertWithHash ::
  Hardware h =>
  Int -> ((DFNode h, [Node]), Node, [Node]) -> CGNodeMap h -> CGNodeMap h
insertWithHash key val map =
  IM.alter (\case
               Nothing -> Just [val]
               Just vals -> Just (val:vals)) key map
