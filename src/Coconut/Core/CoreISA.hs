{-# LANGUAGE RankNTypes #-}
-- |
-- Module      :  Coconut.Core.CoreISA
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a DSL for a generic Instruction Set Architecture (ISA), i.e,
-- it implements a variety of instructions that are included in just about every
-- ISA and therefore useful to use as a foundation for larger / more specific ISA's

module Coconut.Core.CoreISA where

import GHC.TypeLits (Nat,KnownNat)
import Data.Word
import Data.Int

import Coconut.BaseTypes

-- TODO: Temporary imports for instructions that really should
-- be in Z_ISA
import Coconut.Utils.ArbFloat

-- |Finally tagless style DSL for the core Coconut Instruction Set Architecture (ISA)
class CoreISA repr where
{-
Getting values into GPRs
-}
  -- | Stores an unsigned integer value in a general purpose register.
  unintegerG :: Word64 -> repr GPR
  -- | Stores an unsigned 64-bit integer in a general purpose register.
  unwrd64 :: Word64 -> repr GPR
  -- | Stores a list of words in a general purpose register.
  unwrdsG :: [Word32] ->  repr GPR
{-
Getting values into CRs
-}
  -- | Stores an unsigned 64-bit integer in a conditional register.
  unwrd64CR :: Word64 -> repr CR
  -- | Stores an unsigned integer value in a conditional register.
  unintegerCR :: Integer -> repr CR
  -- | Converts a gpr into a conditional register.
  gprToCR :: repr GPR -> repr CR
  -- | Compares two gpr's and stores the result into a conditional register
  cmpGPRs :: repr GPR -> repr GPR -> repr CR -- TODO move to a different section
  -- | Compares two vr's and stores the result into a conditional register
  cmpVRs :: repr VR -> repr VR -> repr CR -- TODO move to a different section
{-
Getting values into VRs
-}
  -- | Stores a list of words in a vector register.
  unwrds      :: [Word32] -> repr VR
  -- | Stores a list of signed int32s in a vector register.
  unint32s    :: [Int32] -> repr VR
  -- | Stores a list of halfwords in a vector register.
  unshorts    :: [Word16] -> repr VR
  -- | Stores a list of int16s in a vector register.
  unint16s    :: [Int16] -> repr VR
  -- | Stores a list of bytes in a vector register.
  unbytes     :: [Word8] -> repr VR
  -- | Stores a list of int8s in a vector register.
  unint8s     :: [Int8] -> repr VR
  -- | Stores a list of bits in a vector register.
  unbits      :: [Word8] -> repr VR
  -- | Stores a list of nibbles (half bytes) in a vector register.
  unnibbles   :: [Word8] -> repr VR
  -- | Stores a list of single-precision floating point numbers in a vector register.
  unfloats    :: [Double] -> repr VR

  -- | Stores an unsigned integer value in a vector register.
  uninteger    :: Integer -> repr VR
  -- | Stores a signed integer value in a vector register.
  unintegerS   :: Integer -> repr VR
  -- | Stores a list of doublewords in a vector register.
  undwrds      :: [Word64] -> repr VR
  -- | Shorthand for `undwrds`, with the list being a single repeating value.
  undwrds2    :: Word64 -> repr VR
  undwrds2 = undwrds . replicate 2
  -- | Stores a list of Int64s in a vector register.
  unint64s     :: [Int64] -> repr VR
  -- | Stores a list of double-precision floating point numbers in a vector register.
  undoubles    :: [Double] -> repr VR

  -- | Shorthand for `undoubles`, with the list being a single repeating value.
  undoubles2  :: Double -> repr VR
  undoubles2 = undoubles . replicate 2

  -- | Shorthand for `unbytes`, with the list being a single repeating value.
  unbytes16   :: Word8 -> repr VR
  unbytes16 = unbytes . replicate 16

  -- | Shorthand for `unwrds`, with the list being a single repeating value.
  unwrds4   :: Word32 -> repr VR
  unwrds4 = unwrds . replicate 4

  -- | Shorthand for `unshorts`, with the list being a single repeating value.
  unshorts8   :: Word16 -> repr VR
  unshorts8 = unshorts . replicate 8
{-
 Retrieving values from GPRs
-}
  -- | Reads a value from a general purpose register as an unsigned 64-bit integer.
  unsignedG :: repr GPR -> Word64
  -- | Reads a value from a general purpose register as a signed integer.
  signedG   :: repr GPR -> Int64
  -- | Reads a value from a general purpose register as a doubleword.
  wrd64     :: repr GPR -> Word64
  -- | Reads values from a general purpose register as a list of words.
  wrdsG     :: repr GPR -> [Word32]
{-
 Retrieving values from VRs
-}
  -- | Reads a value from a vector register as an unsigned integer.
  integer    :: repr VR -> Integer
  -- | Reads a value from a vector register as a signed integer.
  integerS   :: repr VR -> Integer
  -- | Reads a value from a vector register as a hex value, represented as a string.
  hexval     :: repr VR -> String
--   -- | Reads a value from a vector register as a hex value, represented as a string.
--   strings    :: repr VR -> String
  -- | Reads values from a vector register as a list of doublewords.
  dwrds      :: repr VR -> [Word64]
  -- | Reads values from a vector register as a list of signed int64s
  int64s     :: repr VR -> [Int64]
  -- | Reads values from a vector register as a list of words.
  wrds       :: repr VR -> [Word32]
  -- | Reads values from  a vector register as a list of signed int32s.
  int32s     :: repr VR -> [Int32]
  -- | Reads values from a vector register as a list of halfwords.
  shorts     :: repr VR -> [Word16]
  -- | Reads values from a vector register as a list of signed int16s.
  int16s     :: repr VR -> [Int16]
  -- | Reads values from a vector register as a list of bytes.
  bytes      :: repr VR -> [Word8]
  -- | Reads values from a vector register as a list of signed int8s.
  int8s      :: repr VR -> [Int8]
  -- | Reads values from a vector register as a list of nibbles (half bytes).
  nibbles    :: repr VR -> [Word8]
  -- | Reads values from a vector register as a list of bits.
  bits       :: repr VR -> [Word8]
  -- | Reads values from a vector register as a list of single-precision floating point numbers.
  floats     :: repr VR -> [Double]
  -- | Reads values from a vector register as a list of double-precision floating point numbers.
  doubles    :: repr VR -> [Double]

 {- Spilling
  -}
  spillVR :: repr VR -> repr SPILL
  despillVR :: repr SPILL -> repr VR
  spillGPR :: repr GPR -> repr SPILL
  despillGPR :: repr SPILL -> repr GPR

  -- scratchLoadVR  :: repr MR  -> Int -> (repr VR, repr MR)
  -- scratchStoreVR  :: repr MR -> Int -> repr VR -> repr MR

  {- Generic Move Instructions
   -}
  movVR :: repr VR -> repr VR
  movGPR :: repr GPR -> repr GPR

{- Memory (load/store) instructions
 -}
  initMR  :: Integer -> String -> [Interp VR] -> repr MR
  incMR :: repr MR -> repr GPR -> repr MR
  incMR0 :: repr MR -> Int -> repr MR

  -- TODO add incMR ?
  -- incMR   :: repr MR -> Int                          -> repr MR

  -- double precision gpr load/store
  moduloDWLoad :: repr MR -> repr MR -> Int -> (repr GPR, repr MR)
  moduloDWStore :: repr MR -> repr MR -> Int -> repr GPR -> repr MR

  -- vector modulo loads
  moduloVLoad :: repr MR -> repr MR -> Int -> (repr VR, repr MR)
  moduloVStore :: repr MR -> repr MR -> Int -> repr VR -> repr MR


  -- TODO move moduloVLoadG/moduloVStoreG into ZISA
  moduloVLoadG  :: repr MR -> Int -> repr GPR -> (repr VR, repr MR)
  moduloVStoreG  :: repr MR -> Int -> repr GPR -> repr VR -> repr MR

  -- TODO add old modulo loads/stores with extra reg
  -- moduloVLoad :: repr MR -> Integer -> repr GPR -> (repr VR, repr MR)
  -- moduloVStore :: repr MR -> Int -> repr GPR -> repr VR -> repr MR
  -- TODO add moduloGLoad and moduloGStore

  {- Generic logical/arithmetic instructions
   -}
  -- | Bitwise OR of two 64-bit general-purpose registers (OG)
  orG     :: repr GPR -> repr GPR -> repr GPR

  -- | Bitwise exclusive OR of two 64-bit general-purpose registers (XG)
  xorG    :: repr GPR -> repr GPR -> repr GPR

  -- | Bitwise AND of two 64-bit general-purpose registers (NG)
  andG    :: repr GPR -> repr GPR -> repr GPR

  -- | Addition of two 64-bit general-purpose registers (AGRK)
  addG    :: repr GPR -> repr GPR -> repr GPR

  -- | Subtraction of two 64-bit general-purpose registers (AGRK)
  subG    :: repr GPR -> repr GPR -> repr GPR

  -- TODO expand generic logical/arithmetic instructions

  {- Generic branching instructions
   -}

  -- | Jump on equal
  jump :: repr BRANCH

  -- | Compare and branch not low (i.e., >=)
  branchNotLow :: repr GPR -> repr GPR  -> repr BRANCH
  -- | Compare and branch low (i.e., <)
  branchLow :: repr GPR -> repr GPR  -> repr BRANCH
  -- | Compare and branch not equal
  branchImmNotEq :: repr GPR -> Int  -> repr BRANCH
  -- | Compare and branch less then or equal
  branchImmNotHigh :: repr GPR -> Int  -> repr BRANCH

  -- TODO expand core branching instructions
