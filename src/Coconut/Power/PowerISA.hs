{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- Module      :  Coconut.Power.PowerISA
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a DSL for the IBM Power Instruction Set Architecture (ISA), i.e,

module Coconut.Power.PowerISA where

import Data.Word (Word64, Word32, Word16, Word8)
import qualified Data.List as List
import qualified Data.ByteString as BS
import GHC.Arr (inRange)
import GHC.TypeLits
import Data.Proxy (Proxy(..))
import Numeric (showHex)
import Data.List (intercalate)

import Coconut.BaseTypes
import Coconut.Utils.RunTimeSized
import Data.Int (Int64, Int8, Int16, Int32)
import Coconut.Utils.ArbFloat (dbl2Word32, dbl2Word64, chop, word322Dbl, word642Dbl, saturate, FPClass(..), af2DVal, dval2af)

import Data.Bits as Bits (xor, rotateL, complement, shift, shiftR, shiftL, (.&.), rotate, (.|.))
import Data.List.Split (chunksOf)
import Coconut.Utils.ArbFloat

-- | Finally tagless style DSL for the Z Coconut Instruction Set Architecture (ISA)
class PowerISA repr where

-- || FUNCTIONS TO CONVERT BETWEEN GPR AND OTHER TYPES || --

  -- | Stores an unsigned integer value in a general purpose register.
  unintegerG :: Word64 -> repr GPR
  -- | Stores an signed integer value in a general purpose register.
  unintegerSG :: Integer -> repr GPR

  -- | Reads a value from a general purpose register as a signed integer.
  signedG   :: repr GPR -> Int64
  -- | Reads a value from a general purpose register as an unsigned 64-bit integer.
  unsignedG :: repr GPR -> Word64



-- || FUNCTIONS TO DO ARITHMETIC || --

  -- | Takes the absolute value of the contents of a general-purpose register and places the result in another general-purpose register.
  -- | Name: Absolute
  -- | Function: abs
  absG :: repr GPR -> repr GPR
  
  -- | Adds the contents of two general-purpose registers and places the result in a general-purpose register
  -- | Name: Add Carrying 
  -- | Function: addc
  addcG :: repr GPR -> repr GPR -> repr GPR
  
  -- | Calculates an address from an offset and a base address and places the result in a general-purpose register.
  -- | Name: Add Immediate / Compute Address Lower
  -- | Function: addi / cal
  addiG :: repr GPR -> Int -> repr GPR
  
  -- | Subtracts the contents of two general-purpose registers and places the result in a third general-purpose register.
  -- | Name: Subtract From
  -- | Function: subf
  subfG   :: repr GPR -> repr GPR -> repr GPR
  
  -- | Subtracts the contents of a general-purpose register from a 16-bit signed integer and places the result in another general-purpose register.
  -- | Name: Subtract from Immediate Carrying
  -- | Function: subfic / sfi
  subficG   :: repr GPR -> Int -> repr GPR
  
  -- | Negation of a 64-bit general-purpose register
  -- | Name: Negate
  -- | Function: neg
  negG    :: repr GPR -> repr GPR
  
  -- | Signed multiplication of two 64-bit values. Place the high-order 64 bits of the result into a register.
  -- | Name: Multiply High Double Word
  -- | Function: mulhd
  mulhdG  :: repr GPR -> repr GPR -> repr GPR
  
  -- | Signed multiplcation of two 64-bit general-purpose registers. Place the low-order 64 bits of the result into a register.
  -- | Name: Multiply Low Double Word
  -- | Function: mulld
  mulldG  :: repr GPR -> repr GPR -> repr GPR
  
  -- | Unsigned multiplication of two general-purpose registers. Place the high-order 64 bits of the result into a register.
  -- | Name: Multiply High Double Word Unsigned
  -- | Function: mulhdu
  mulhduG :: repr GPR -> repr GPR-> repr GPR
  
  -- | Signed division of the contents of a general purpose register by the contents of a general purpose register, storing the result into a general purpose register.
  -- | Name: Divide Double Word
  -- | Function: divd
  divdG   :: repr GPR -> repr GPR -> repr GPR

  -- | Unsigned division of the contents of a general purpose register by the contents of a general purpose register, storing the result into a general purpose register.
  -- | Name: Divide Double Word Unsigned
  -- | Function: divdu
  divduG  :: repr GPR -> repr GPR -> repr GPR


-- ******GO THROUGH THESE AND MAKE SURE THEY MAKE SENSE *************
  -- | Rotate the contents of a general purpose register left by the number of bits specified by an immediate value. Clear a specified number of high-order bits. Place the results in another general purpose register.
  -- | Name: Rotate Left Double Word Immediate then Clear Left
  -- | Function: rldicl
  rldiclG :: repr GPR -> Integer -> Integer -> repr GPR

  -- | TODO: find equivalent instruction, if any
   -- | Rotate the contents of a general purpose register left by the number of bits specified by an immediate value. Clear a specified number of low-order bits. Place the results in another general purpose register.
   -- | Name: Rotate Left Double Word Immediate then Clear Right
   -- | Function: rldicr
  rldicrG :: repr GPR -> Integer -> Integer -> repr GPR

  -- | TODO: find equivalent instruction, if any
   -- | The contents of a general purpose register are rotated left a specified number of bits. A generated mask is used to insert a specified bit-field into the corresponding bit-field of another general purpose register.
   -- | Name: Rotate Left Double Word Immediate then Mask Insert
   -- | Function: rldimi
  rldimiG :: repr GPR {- inOut -} -> repr GPR -> Integer -> Integer -> repr GPR

  -- | TODO: find equivalent instruction, if any
   -- | The contents of a general purpose register are rotated left a specified number of bits, then masked with a bit-field to clear some number of low-order and high-order bits. The result is placed in another general purpose register.
   -- | Rotate Left Double Word Immediate then Clear
   -- | Function: rldic
  rldicG  :: repr GPR -> Integer -> Integer -> repr GPR

  -- | TODO: find equivalent instruction, if any
   -- | Rotate the contents of a general purpose register left by the number of bits specified by the contents of another general purpose register. Generate a mask that is ANDed with the result of the shift operation. Store the result of this operation in another general purpose register.
   -- | Rotate Left Double Word then Clear Left
   -- | Function: rldcl
  rldclG  :: repr GPR -> repr GPR -> Integer -> repr GPR

  -- | TODO: find equivalent instruction, if any
   -- | Rotate the contents of a general purpose register left by the number of bits specified by the contents of another general purpose register. Generate a mask that is ANDed with the result of the shift operation. Store the result of this operation in another general purpose register.
   -- | Rotate Left Double Word then Clear Right
   -- | Function: rldcr
  rldcrG  :: repr GPR -> repr GPR -> Integer -> repr GPR

  -- | TODO: find equivalent instruction, if any
   -- | Rotates the contents of a general-purpose register to the left by a specified number of bits and stores the result in another general-purpose register under the control of a generated mask.
   -- | Rotate Left Word Immediate Then Mask Insert
   -- | Function: rlwimi / rlimi 
  rlwimiG :: repr GPR {- inOut -} -> repr GPR -> Integer -> Integer -> Integer -> repr GPR

  -- | Adds the contents of two general-purpose registers
  -- | Name: Add
  -- | Function: add
  add :: repr GPR -> repr GPR -> repr GPR

  -- | XORs the contents of two general-purpose registers and places the result in another general-purpose register
  -- | Name: xor
  -- | Function: xorG
  xorG :: repr GPR -> repr GPR -> repr GPR

  -- | XORs the lower 16 bits of a general-purpose register with a 16-bit unsigned integer and places the result in another general-purpose register
  -- | Name: xori
  -- | Function: xoriG
  xoriG :: repr GPR -> Word -> repr GPR

  -- | Logically XORs the contents of two general-purpose registers and places the complemented result in a general-purpose register
  -- | Name: eqv
  -- | Function: eqvG
  eqvG :: repr GPR -> repr GPR -> repr GPR

  -- | Logically complements the result of ANDing the contents of two general-purpose registers and stores the result in another general-purpose register
  -- | Name: nand
  -- | Function: nandG
  nandG :: repr GPR -> repr GPR -> repr GPR

  -- | Logically ORs the contents of two general-purpose registers and stores the result in another general-purpose register
  -- | Name: or
  -- | Function: orG
  orG :: repr GPR -> repr GPR -> repr GPR

  -- | Logically complements the result of ORing the contents of two general-purpose registers and stores the result in another general-purpose register
  -- | Name: nor
  -- | Function: norG
  norG :: repr GPR -> repr GPR -> repr GPR

  -- | XORs the upper 16 bits of a general-purpose register with a 16-bit unsigned integer and places the result in another general-purpose register
  -- | Name: xoris
  -- | Function: xorisG
  -- xorisG :: repr GPR -> Int -> repr GPR

  -- | Compares the contents of two general-purpose registers algebraically
  -- | Name: cmp
  -- | Function: cmp
  cmp :: repr GPR -> repr GPR -> repr GPR -> repr GPR

  -- | Divide the contents of a general purpose register by the contents of a general purpose register, storing the result into a general purpose register
  -- | Name: divdu
  -- | Function: divdu
  divdu :: repr GPR -> repr GPR -> repr GPR

  -- | Logically ANDs the contents of two general-purpose registers and places the result in a general-purpose register
  -- | Name: and
  -- | Function: andG
  andG :: repr GPR -> repr GPR -> repr GPR

  -- | Logically ANDs the contents of a general-purpose register with the complement of the contents of a general-purpose register
  -- | Name: andc
  -- | Function: andc
  andc :: repr GPR -> repr GPR -> repr GPR

  -- | Places the complemented result of XORing two Condition Register bits in a Condition Register bit
  -- | Name: creqv
  -- | Function: creqv
  creqv :: repr CR -> repr CR -> repr CR

  -- | Places the result of ANDing two Condition Register bits in a Condition Register bit
  -- | Name: crand
  -- | Function: crand
  crand :: repr CR -> repr CR -> repr CR

  -- | Places the result of ANDing one Condition Register bit and the complement of a Condition Register bit in a Condition Register bit
  -- | Name: crandc
  -- | Function: crandc
  crandc :: repr CR -> repr CR -> repr CR

  -- | Places the complemented result of ANDing two Condition Register bits in a Condition Register bit
  -- | Name: crnand
  -- | Function: crnand
  crnand :: repr CR -> repr CR -> repr CR

  -- | Places the result of ORing two Condition Register bits in a Condition Register bit
  -- | Name: crnor
  -- | Function: crnor
  crnor :: repr CR -> repr CR -> repr CR

  -- | Places the result of XORing two Condition Register bits in a Condition Register bit
  -- | Name: crxor
  -- | Function: crxor
  crxor :: repr CR -> repr CR -> repr CR

  -- | Places the result of ORing a Condition Register bit and the complement of a Condition Register bit in a Condition Register bit
  -- | Name: crorc
  -- | Function: crorc
  crorc :: repr CR -> repr CR -> repr CR

  -- | Logically XORs the contents of two general-purpose registers and places the complemented result in a general-purpose register
  -- | Name: eqv
  -- | Function: eqv
  eqv :: repr GPR -> repr GPR -> repr GPR


-- || FUNCTIONS TO COUNT LEADING ZEROS || --
  
  -- | Count the number of consecutive zero bits in the contents of a general purpose register, beginning with the high-order bit.
  -- | Function: cntlzd
  cntlzd :: repr GPR -> repr GPR

  -- | Counts the number of leading zeros of the 32-bit value in a source general-purpose register (GPR) and stores the result in a GPR.
  -- | Function: cntlzw
  cntlzw :: repr GPR -> repr GPR

-- || VECTOR FUNCTIONS || --
{-
Retrieving values from VRs
-}
  -- | Reads a value from a vector register as an unsigned integer.
  integer    :: repr VR -> Integer
  -- | Reads a value from a vector register as a signed integer.
  integerS   :: repr VR -> Integer
  -- | Reads a value from a vector register as a hex value, represented as a string.
  hexval     :: repr VR -> String
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

{-
Getting values into VRs
-}
  -- | Stores a list of words in a vector register.
  unwrds      :: [Word32] -> repr VR
  -- | Stores a list of integers in a vector register.
  unwrds'     :: [Word32] -> repr VR
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
  -- | Stores a list of Int64s in a vector register.
  unint64s     :: [Int64] -> repr VR
  -- | Stores a list of double-precision floating point numbers in a vector register.
  undoubles    :: [Double] -> repr VR
  -- | Shorthand for `undoubles`, with the list being a single repeating value.
  undoubles2  :: Double -> repr VR
  -- | Shorthand for `unbytes`, with the list being a single repeating value.
  unbytes16   :: Word8 -> repr VR
  -- | Shorthand for `unwrds`, with the list being a single repeating value.
  unwrds4   :: Word32 -> repr VR
  -- | Shorthand for `unshorts`, with the list being a single repeating value.
  unshorts8   :: Word16 -> repr VR

{-
Vector Select Instructions 
-}

  {-
     Vector select (VSEL)

     For each bit in the third operand that contains a zero, the corresponding bit from the second
     operand is placed in the corresponding bit of the output register. For each bit in the third
     operand that contains a one, the corresponding bit from the first operand is placed in the
     corresponding bit of the output register.
  -}
  xxsel :: repr VR -> repr VR -> repr VR -> repr VR

{-
Vector Permute Instructions 
-}

  {- 
     Vector Permute (VPERM)

    The Vector Permute instruction allows any byte in two source VSRs to be copied to any byte in the target VSR. The
    bytes in a third source VSR specify from which byte in the first two source VSRs the corresponding target byte is to
    be copied. The contents of the third source VSR are sometimes referred to as the “permute control vector”.
  -}
  xxperm :: repr VR -> repr VR -> repr VR -> repr VR

  {- 
     Vector Permute Right-indexed (VPERMR)

    The Vector Permute instruction allows any byte in two source VSRs to be copied to any byte in the target VSR. The
    bytes in a third source VSR specify from which byte in the first two source VSRs the corresponding target byte is to
    be copied. The contents of the third source VSR are sometimes referred to as the “permute control vector”.
  -}
  xxpermr :: repr VR -> repr VR -> repr VR -> repr VR

{-
Vector Merge Instructions 
-}

  {- 
     Vector Merge High Byte (VMRGHB)

     Merges the first half of two vectors interpreting each as a list of a bytes.
  -}
  vmrghb :: repr VR -> repr VR -> repr VR

  {- 
     Vector Merge Low Byte (VMRGLB)

     Merges the second half of two vectors interpreting each as a list of a bytes.
  -}
  vmrglb :: repr VR -> repr VR -> repr VR

  {- 
     Vector Merge High Halfword (VMRGHH)

     Merges the first half of two vectors interpreting each as a list of a halfwords (shorts).
  -}
  vmrghh :: repr VR -> repr VR -> repr VR

  {- 
     Vector Merge Low Halfword (VMRGLH)

     Merges the second half of two vectors interpreting each as a list of a halfwords (shorts).
  -}
  vmrglh :: repr VR -> repr VR -> repr VR

  {- 
     Vector Merge High Word (VMRGHW)

     Merges the first half of two vectors interpreting each as a list of a words.
  -}
  vmrghw :: repr VR -> repr VR -> repr VR

  {- 
     Vector Merge Low Word (VMRGLW)

     Merges the second half of two vectors interpreting each as a list of a words.
  -}
  vmrglw :: repr VR -> repr VR -> repr VR

  {- 
     Vector Merge Even Word (VMRGEW)

     Merges two vectors interpreting each as a list of a words such that VA[0] -> VT[0], VB[0] -> VT[1], VA[2] -> VT[2], VB[2] -> VT[3].
  -}
  vmrgew :: repr VR -> repr VR -> repr VR

  {- 
     Vector Merge Odd Word (VMRGOW)

     Merges two vectors interpreting each as a list of a words such that VA[1] -> VT[0], VB[1] -> VT[1], VA[3] -> VT[2], VB[3] -> VT[3].
  -}
  vmrgow :: repr VR -> repr VR -> repr VR

  {-|
    Vector count leading zeros

    For each element in the second operand, the number of leftmost zeros are counted and placed in
    the corresponding element of the first operand

    The bits of each element in the second operand are scanned left to right for the leftmost one bit.
    A binary integer designating the bit position of the leftmost one bit, or the number of
    bits in the element if there is no one bit, is placed in the corresponding element of the
    first operand.

    The first input determines the element size:

      * 0    -> Byte
      * 1    -> Halfword
      * 2    -> Word
      * 3    -> Doubleword
      * 4-15 -> Reserved
  -}


{-
Vector Count Leading Zeros Instructions 
-}

   {- 
     Vector Count Leading Zeros Byte (VCLZB)

     Counts the leading zeros of a vector interpreted as a list of bytes.
  -}
  vclzb :: repr VR -> repr VR

  {- 
     Vector Count Leading Zeros Halfword (VCLZH)

     Counts the leading zeros of a vector interpreted as a list of halfwords.
  -}
  vclzh :: repr VR -> repr VR

  {- 
     Vector Count Leading Zeros Byte (VCLZW)

     Counts the leading zeros of a vector interpreted as a list of words.
  -}
  vclzw :: repr VR -> repr VR

  {- 
     Vector Count Leading Zeros Byte (VCLZD)

     Counts the leading zeros of a vector interpreted as a list of double words.
  -}
  vclzd :: repr VR -> repr VR

  {- 
     Vector Count Leading Zeros Byte (VCLZDM)

     Counts the leading zeros of a vector ra interpreted as a list of double words under a bit mask of rb.
  -}
  vclzdm :: repr VR -> repr VR -> repr VR


{-
Vector Floating Point Arithmetic Instructions 
-}

   {- 
     Floating Multiply-Add Double Precision (xvadddp)

     Takes the product of two floating point operands then adds another floating point to the result.
  -}
  xvmaddmdp :: repr VR -> repr VR -> repr VR -> repr VR
  
   {- 
     Floating Multiply-Subtract Double precision (xvsubdp)

     Takes the product of two floating point operands then subtracts the result with another floating point.
  -}
  xvmsubmdp :: repr VR -> repr VR -> repr VR -> repr VR

  {- 
     Floating Negative Multiply-Add Double precision (xvnmaddmsp)

     Multiplies 2 floating point operands and adds another floating point to the intermediate result. The result 
     is then negated.
  -}
  xvnmaddmdp :: repr VR -> repr VR -> repr VR -> repr VR

  {- 
     Floating Negative Multiply-Add Double precision (xvnmsubmsp)

     Multiplies 2 floating point operands and subtracts another floating point to the intermediate result. The result 
     is then negated.
  -}
  xvnmsubmdp :: repr VR -> repr VR -> repr VR -> repr VR

   {- 
     Vector Logical AND VX-form (vand)

     Returns the bitwise AND of the two inputs.
  -}
  vand           :: repr VR -> repr VR -> repr VR  -- VN

  {- 
     Vector Logical OR VX-form (vor)

     Returns the bitwise OR of the two inputs.
  -}
  vor :: repr VR -> repr VR -> repr VR  -- VO

  {- 
     Vector Logical OR with Complement VX-form (vorc)

     Returns the bitwise OR of the first input and the complement of the second input.
  -}
  vorc :: repr VR -> repr VR -> repr VR  -- VNO

  {- 
     Vector Logical XOR VX-form (vxor)

     Returns the bitwise XOR of the two inputs.
  -}
  vxor :: repr VR -> repr VR -> repr VR  -- VX

  {- 
     Vector Logical XOR with Complement VX-form (vxorc)

     Returns the bitwise XOR of the first input and the complement of the second input.
  -}
  vxorc :: repr VR -> repr VR -> repr VR

  {- 
     Vector Logical AND with Complement VX-form (vandc)

     Returns the bitwise AND of the first input and the complement of the second input.
  -}
  vandc          :: repr VR -> repr VR -> repr VR  -- VNC

  {- |
     Vector floating-point addition (VFA)

     The floating-point elements of the first vector are added to the floating-point elements
     of the second vector.
  -}
  xvadddp            :: repr VR -> repr VR -> repr VR

  {- |
     Vector floating-point subtract (VFS)

     The floating-point elements of the second vector are subtracted from the floating-point elements
     of the first vector.
  -}
  xvsubdp            :: repr VR -> repr VR -> repr VR

  {- |
     Vector floating-point multiply (VFM)

     The floating-point elements of the first vector are multiplied by the floating-point elements
     of the second vector.
  -}
  xvmuldp            :: repr VR -> repr VR -> repr VR


  dwrdOp2 :: (Word64 -> Word64 -> Word64) -> repr VR -> repr VR -> repr VR
  doubleOp2 :: (ArbFloat -> ArbFloat -> ArbFloat) -> repr VR -> repr VR -> repr VR
  doubleOp3 :: (ArbFloat -> ArbFloat -> ArbFloat -> ArbFloat) -> repr VR -> repr VR -> repr VR -> repr VR
  unafdoubles :: [ArbFloat] -> repr VR
  afdoubles :: repr VR -> [ArbFloat]


   {- 
     Floating Add Single Prcision (xvaddsp)

     Takes a floating point then adds another floating point to it.
  -}
  xvaddsp :: repr VR -> repr VR -> repr VR

  {- 
     Floating Subtract Single Prcision (xvsubsp)

     Takes a floating point then subtracts another floating point from it.
  -}
  xvsubsp :: repr VR -> repr VR -> repr VR

  {- 
     Floating Multiply Single Prcision (xvmulsp)

     Takes a floating point then multiplies another floating point with it.
  -}
  xvmulsp :: repr VR -> repr VR -> repr VR

   {- 
     Floating Multiply-Add Single Prcision (xvadddp)

     Takes the product of two floating point operands then adds another floating point to the result.
  -}
  xvmaddmsp :: repr VR -> repr VR -> repr VR -> repr VR

   {- 
     Floating Multiply-Add Single Prcision (xvadddp)

     Takes the negative product of two floating point operands then adds another floating point to the result.
  -}
  xvnmaddmsp :: repr VR -> repr VR -> repr VR -> repr VR

   {- 
     Floating Multiply-Sub Single Prcision (xvadddp)

     Takes the product of two floating point operands then subtracts the result with another floating point.
  -}
  xvmsubmsp :: repr VR -> repr VR -> repr VR -> repr VR

   {- 
     Floating Multiply-Sub Single Prcision (xvadddp)

     Takes the negative product of two floating point operands then subtracts the result with another floating point.
  -}
  xvnmsubmsp :: repr VR -> repr VR -> repr VR -> repr VR

  floatOp2 :: (Double -> Double -> Double) -> repr VR -> repr VR -> repr VR
  floatOp3 :: (Double -> Double -> Double -> Double) -> repr VR -> repr VR -> repr VR -> repr VR

{- Functions from Old PowerInterp.lhs-}
--   c2flt :: (HasSpuValue node, PowerType node) => (Integer -> Integer) -> node -> Integer -> node
  csflt :: forall repr. PowerISA repr => repr VR -> Integer -> repr VR

{- Functions to replace old instructions -}
-- VSX Vector Convert with round
-- Double-Precision to Single-Precision format
-- XX2-form
  xvcvdpsp :: {-forall repr. PowerISA repr =>-} repr VR -> repr VR

   -- TODO: Overwrites first register
  vrlqmi :: repr VR -> repr VR -> repr VR -> repr VR

  vcmpgtub :: repr VR -> repr VR -> repr VR

  vcmpgtfp :: repr VR -> repr VR -> repr VR