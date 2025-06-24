-- |
-- Module      :  CodeGraph
-- Copyright   :  (c) OCA 2021
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module provides a DataFlow and ControlFlow graph's built on top of @HyperGraph@
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}


module Coconut.Graph.ByteStringAST where

import Coconut.BaseTypes

import Data.ByteString.Internal (c2w)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Char8 as BC

import qualified Data.List as List

import Control.DeepSeq (force)

sepW8 = BS.toShort $ BC.singleton '_'
lBracketW8 = BS.toShort $ BC.singleton '('
rBracketW8 = BS.toShort $ BC.singleton ')'
indexW8 = BS.toShort $ BC.singleton ']'

initMRW8 = BS.toShort $ BC.pack "initMR"
meetW8 = BS.toShort $ BC.pack "meet"
joinW8 = BS.toShort $ BC.pack "join"

s2b :: String -> ShortByteString
s2b = BS.pack . map c2w

buildByteStringAST :: DFNode h -> [ShortByteString] -> ShortByteString
buildByteStringAST !dfNode !inpASTs =
  let
    opLbl :: ShortByteString
    !opLbl = case dfNode of
              (InstructionNode (Instruction imm name _) ) -> (s2b name) <> (s2b $ show imm)
              (InstructionNode (Move name _) ) -> (s2b name)
              (InstructionNode (Spill name) ) -> (s2b name)
              (InstructionNode (Despill name) ) -> (s2b name)
              -- TODO how to handle initmr tables
              (InstructionNode (InitMR name _) ) -> initMRW8<>(s2b name)
              (InstructionNode Meet) -> meetW8
              (InstructionNode Join) -> joinW8
              (BranchNode name imm _) -> (s2b name)<>(s2b $ show imm)
              (ResourceNode _) -> error "buildStringAST given ResourceNode"
    !inpLbls' = force $ mconcat $ map (\s -> s<>rBracketW8) inpASTs
  in force $ inpLbls' <> sepW8 <> opLbl

buildASTParams ast n = force $ ast <> indexW8 <> (BS.pack [n])
