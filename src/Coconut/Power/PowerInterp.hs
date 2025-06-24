-- |
-- Module      :  Coconut.Core.Interp
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a functionality for interpreting the @CoreISA@ DSL

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, ExistentialQuantification, TypeFamilies, TypeApplications, ScopedTypeVariables, InstanceSigs, DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes, TupleSections #-}


module Coconut.Power.PowerInterp where

import qualified Data.List as List
import qualified Data.ByteString as BS

import Coconut.BaseTypes
import Coconut.Power.PowerISA
import Coconut.Utils.RunTimeSized
import Coconut.Utils.ArbFloat (dbl2Word32, dbl2Word64, chop, word322Dbl, word642Dbl, saturate, FPClass(..), af2DVal, dval2af)

import Data.Int (Int64, Int8, Int16, Int32)
import Data.Word (Word64, Word32, Word16, Word8)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Bits as Bits (xor, rotateL, complement, shift, shiftR, shiftL, (.&.), rotate, (.|.), countLeadingZeros)
import Data.Proxy (Proxy(..))

import GHC.Arr (inRange)
import GHC.TypeLits

import Numeric (showHex)

-- || HELPER FUNCTIONS FROM OLD PowerInterp.lhs|| --


-- unsign :: (Integral b, Num a, Ord a, Show b) => b -> a -> a
unsign b v  | b < 1     = error $ "PPCAC.unsign bit "++show b
            | v < 0     = 2^b + v
            | otherwise = v

sign b v  | b < 1         = error $ "PowerInterp.sign neg bit "++show b
          | v >= 2^(b-1)  = v - 2^b
          | otherwise     = v

-- Convert to float
c2flt sgnFun v exp = unfloats $ map (scale . fromInteger . sgnFun . fromIntegral) $ wrds v
  where scale | 0 <= exp && exp < 128 = ( * (2**(- (fromInteger exp))) )
              | otherwise               = error "exponent out of range"

cflts v exp = unwrds' $ map ((unsign 32) . unflt' . scale) $ floats v
      where exp' = exp -- 173 - exp
            scale x | 0 <= exp' && exp' < 128 = x * (2**( (fromInteger exp')))
                    | otherwise               = error "cflts undefined"
            unflt' x | x < 0.5 - 2**31   = - 2^31
                     | x > 2**31 - 1.5   = 2^31 - 1
                     | otherwise         = floor x

-- || HELPER FUNCTIONS || --


-- [ if n >= x && n <= y then 1 else 0 | n <- [0..127]]
mask128 x y = 
  if x <= y then 2^(128 - x) - 2^(127 - y)
  else 2^(128) - 2^(127 - y) + 2^(128 - x) - 1

rotateLeft, rotateLeft32 :: Word64 -> Word64 -> Word64
rotateLeft ra rb = shiftL ra (fromIntegral rb) .&. (2^64 - 1) .|. shiftR ra (64-fromIntegral rb)
rotateLeft32 ra' rb = let ra = ra' .&. (2^32 - 1)
  in shiftL ra (fromIntegral rb) .&. (2^32 - 1) .|. shiftR ra (32-fromIntegral rb)

pupd f g (x,y) = (f x, g y)
pupdD f = pupd f f

encodeWord64 :: Word64 -> [Word32]
encodeWord64 x = map fromIntegral [ x .&. 0xFFFFFFFF, (x .&. 0xFFFFFFFF00000000) `shiftR` 32 ]

padTruncateTo k x xs = take k $ xs ++ repeat x

safeDivMod x y = if y == 0 then error "divide by zero in safeDivMod" else divMod x y

-- | Merge helper functions 
-- It would be great if somebody could test the mergehigh and mergelow implementations to double check my code!
merge [] [] = []
merge (a1:as) (b1:bs) = a1:b1:merge as bs
merge a b = error $ "ZInterp.mergeHi illegal "++show(a,b)

first_halve :: [a] -> [a]
first_halve = (\xs -> case xs of
            [] -> []
            xs -> take ((length xs) `div` 2 ) xs)

second_halve :: [a] -> [a]
second_halve = (\xs -> case xs of
            [] -> []
            xs -> drop ((length xs) `div` 2 ) xs)

mergeEven [] [] = []
mergeEven (a1:_:as) (b1:_:bs) = a1:b1:mergeEven as bs
mergeEven a b = error $ "ZInterp.mergeEven illegal "++show(a,b)

mergeOdd [] [] = []
mergeOdd (_:a1:as) (_:b1:bs) = a1:b1:mergeOdd as bs
mergeOdd a b = error $ "ZInterp.mergeOdd illegal "++show(a,b)

instance PowerISA Interp where

-- || FUNCTIONS TO CONVERT BETWEEN GPR AND OTHER TYPES || --
  unintegerG  = InterpGPR . fromIntegral
  unintegerSG x = if x < 0
                then unintegerG (fromIntegral(2^64 - Prelude.abs x `mod` 2^64))
                else unintegerG (fromIntegral(x `mod` 2^63))
  
  signedG   = fromIntegral . runInterpGPR
  unsignedG x   = fromIntegral (fromIntegral $ runInterpGPR x :: Int64)

-- || FUNCTIONS TO DO ARITHMETIC || --
  absG ra = unintegerSG $ fromIntegral $ Prelude.abs(signedG ra)

  addcG ra rb = InterpGPR $ runInterpGPR ra + runInterpGPR rb
  addiG ra si = InterpGPR $ runInterpGPR ra + fromIntegral si
  subfG ra rb = InterpGPR $ runInterpGPR ra - runInterpGPR rb
  subficG ra ib = InterpGPR $ runInterpGPR ra - fromIntegral ib
  negG ra = InterpGPR $ negate $ runInterpGPR ra
  mulldG ra rb  = InterpGPR $ runInterpGPR ra * runInterpGPR rb
  mulhdG ra rb  = InterpGPR . fromIntegral . flip div (2^64) $ (signedG ra * signedG rb)
  mulhduG ra rb = InterpGPR . fromIntegral . flip div (2^64) $ (unsignedG ra * unsignedG rb)
  divdG ra rb   = InterpGPR . fromIntegral $ (signedG ra `div` signedG rb)
  divduG ra rb  = InterpGPR . fromIntegral $ (unsignedG ra `div` unsignedG rb)

  rldiclG rs sh mb = InterpGPR $ Bits.rotateL (runInterpGPR rs) (fromIntegral sh)  .&. (2^(63-mb+1) - 1)
  rldicrG rs sh me = InterpGPR $ Bits.rotateL (runInterpGPR rs) (fromIntegral sh)  .&. (2^(63+1) - 2^(63-me))
  rldicG rs sh mb = InterpGPR $ Bits.rotateL (runInterpGPR rs) (fromIntegral sh)  .&. (2^(63+mb+1) - 2^(63-sh))
  rldimiG ra rb sh mb = let mask = (2^(64-mb) - 2^sh)
      in InterpGPR $ Bits.rotateL (runInterpGPR rb) (fromIntegral sh) .&. mask
                  .|. runInterpGPR ra .&. complement mask
  rldclG ra rb = error "ZInterp.rldcl"
  rldcrG ra rb = error "ZInterp.rldcr"
  rlwimiG ra rb sh mb me = let mask = (2^(32-mb) - 2^(31-me)) in InterpGPR $
    fromIntegral $ rotateLeft32 (fromIntegral $ runInterpGPR rb) (fromIntegral sh) .&. mask
               .|. fromIntegral (runInterpGPR ra) .&. complement mask

  cmp ra rb bf = case runInterpGPR bf of 0 -> if runInterpGPR ra < runInterpGPR rb then InterpGPR $ 1 else InterpGPR $ 0
                                         1 -> if runInterpGPR ra > runInterpGPR rb then InterpGPR $ 1 else InterpGPR $ 0
                                         2 -> if runInterpGPR ra == runInterpGPR rb then InterpGPR $ 1 else InterpGPR $ 0

  add ra rb = InterpGPR $ runInterpGPR ra + runInterpGPR rb

  xorG ra rb = InterpGPR $ fromIntegral $ signedG ra `xor` signedG rb
  
  xoriG ra ui = InterpGPR $ fromIntegral $ signedG ra `xor` fromIntegral ui

  eqvG ra rb =  InterpGPR $ complement $ fromIntegral $ signedG ra `xor` signedG rb

  nandG ra rb = InterpGPR $ complement $ fromIntegral $ signedG ra .&. signedG rb

  orG ra rb = InterpGPR $ fromIntegral $ signedG ra .|. signedG rb

  norG ra rb = InterpGPR $ complement $ fromIntegral $ signedG ra .|. signedG rb

  -- xorisG ra ui =
  --   let 
  --     upperBits = signedG ra .&. complement 0xFFFF
  --     xorResult = upperBits .|. (fromIntegral ui .&. 0xFFFF)
  --   in InterpGPR $ fromIntegral xorResult

  
  divdu ra rb  = InterpGPR . fromIntegral $ (unsignedG ra `div` unsignedG rb)

  andG ra rb = InterpGPR $ runInterpGPR ra .&. runInterpGPR rb

  andc ra rb = InterpGPR $ runInterpGPR ra .&. complement (runInterpGPR rb)

  creqv ba bb = InterpCR $ complement (runInterpCR ba `Bits.xor` runInterpCR bb)

  crand ba bb = InterpCR $ runInterpCR ba .&. runInterpCR bb

  crandc ba bb = InterpCR $ runInterpCR ba .&. complement (runInterpCR bb)

  crnand ba bb = InterpCR $ complement (runInterpCR ba .&. runInterpCR bb)

  crnor ba bb = InterpCR $ complement (runInterpCR ba .|. runInterpCR bb)

  crxor ba bb = InterpCR $ runInterpCR ba `Bits.xor` runInterpCR bb

  crorc ba bb = InterpCR $ runInterpCR ba .|. complement (runInterpCR bb)

  eqv rs rb = InterpGPR $ runInterpGPR rs `Bits.xor` runInterpGPR rb

-- || FUNCTIONS TO COUNT LEADING ZEROS || --
  cntlzd rs = unintegerSG $ fromIntegral $ countLeadingZeros $ signedG rs
  cntlzw rs = addcG (unintegerSG ( fromIntegral ( countLeadingZeros (signedG rs .&. 0x00000000FFFFFFFF)))) (unintegerSG (-32))


-- || VECTOR INSTRUCTIONS || --
{- Getting values out of VRs -}
  integer   = (\ (a,b) -> 2^64 * a + b) . pupdD fromIntegral . runInterpVR
  integerS  = (\ (a,b) -> a `div` 2^63 * (-2^127) + (2^64 * (a `mod` 2^63) + b)) . pupdD fromIntegral . runInterpVR
  dwrds     = map fromIntegral . (\ (a,b) -> [a,b]) . runInterpVR
  int64s    = map fromIntegral . (\ (a,b) -> map fromIntegral [a,b]) . runInterpVR
  wrds      = map fromIntegral . uncurry (++) . pupdD (chop 32) . runInterpVR
  int32s    = map fromIntegral . uncurry (++) . pupdD (chop 32) . runInterpVR
  shorts    = map fromIntegral . uncurry (++) . pupdD (chop 16) . runInterpVR
  int16s    = map fromIntegral . uncurry (++) . pupdD (chop 16) . runInterpVR
  bytes     = map fromIntegral . uncurry (++) . pupdD (chop 8) . runInterpVR
  int8s     = map fromIntegral . uncurry (++) . pupdD (chop 8) . runInterpVR
  nibbles   = map fromIntegral . uncurry (++) . pupdD (chop 4) . runInterpVR
  bits      = map fromIntegral . uncurry (++) . pupdD (chop 1) . runInterpVR
  floats    = map word322Dbl . wrds
  doubles   = map word642Dbl . dwrds
  hexval x  =
    let strVal = showHex (integer x) ""
    in  "0x" ++ (unwords $ chunksOf 4 $ replicate (32 - length strVal) '0' ++ strVal)

{- Getting values into GPRs -}
  uninteger   = InterpVR . pupdD fromIntegral . flip safeDivMod (2^64)
  unintegerS x = if x < 0
                 then uninteger (2^128 - abs x `mod` 2^128)
                 else uninteger (x `mod` 2^127)

  undwrds     = InterpVR . (\ [x,y] -> (x,y)) . map fromIntegral . padTruncateTo 2 0
  unwrds      = InterpVR . pupdD (sum . zipWith (*) [2^(32*i) | i <- reverse [0.. 1]])
                    . splitAt  2 . map fromIntegral . padTruncateTo 4 0
  unwrds' = unwrds . map fromIntegral
  undwrds2 = undwrds . replicate 2
  unwrds4 = unwrds . replicate 4
  unshorts8 = unshorts . replicate 8
  unshorts    = InterpVR . pupdD (sum . zipWith (*) [2^(16*i) | i <- reverse [0.. 3]])
                    . splitAt  4 . map ((`mod` (2^16)) . fromIntegral) . padTruncateTo 8 0
  unbytes     = InterpVR . pupdD (sum . zipWith (*) [2^(8*i) | i <- reverse [0.. 7]])
                    . splitAt  8 . map ((`mod` (2^8)) . fromIntegral) . padTruncateTo 16 0
  unbytes16 = unbytes . replicate 16
  unnibbles   = InterpVR . pupdD (sum . zipWith (*) [2^(4*i) | i <- reverse [0..15]])
                    . splitAt 16 . map ((`mod` (2^4)) . fromIntegral) . padTruncateTo 32 0
  unbits      = InterpVR . pupdD (sum . zipWith (*) [2^(1*i) | i <- reverse [0..63]])
                    . splitAt 64 . map ((`mod` (2^1)) . fromIntegral) . padTruncateTo 128 0


  unint64s    = InterpVR . (\ [x,y] -> (x,y)) . map fromIntegral . padTruncateTo 2 0
  unint32s = InterpVR . pupdD (sum . zipWith (*) [2^(32*i) | i <- reverse [0.. 1]]) . splitAt 2 . map ((`mod` (2^32)) . fromIntegral) . padTruncateTo 4 0 
  
  unint16s = InterpVR . pupdD (sum . zipWith (*) [2^(16*i) | i <- reverse [0.. 3]]) . splitAt 4 . map ((`mod` (2^16)) . fromIntegral) . padTruncateTo 8 0 
  
  unint8s = InterpVR . pupdD (sum . zipWith (*) [2^(8*i) | i <- reverse [0.. 7]]) . splitAt 8 . map ((`mod` (2^8)) . fromIntegral) . padTruncateTo 16 0

  unfloats    = unwrds . map dbl2Word32 . padTruncateTo 4 0
  undoubles   = undwrds . map dbl2Word64 . padTruncateTo 2 0
  undoubles2 = undoubles . replicate 2

{- Vector Select Intructions -}
  xxsel ra rb rc = undwrds $ zipWith3 (\ a b sel -> sel .&. b .|. complement sel .&. a)
                                     (dwrds ra) (dwrds rb) (dwrds rc)

{- Vector Permute Intructions -}
  xxperm ra rb rc = unbytes $ map shufb' $ bytes rc
    where
      all = bytes ra ++ bytes rb
      shufb' x = all !! fromIntegral (mod x 32)
  
  xxpermr ra rb rc = unbytes $ map shufb' $ bytes rc
    where
      all = bytes ra ++ bytes rb
      shufb' x = all !! (31 - fromIntegral (mod x 32))

{- Vector Permute Intructions -}
  vclzb ra = unbytes $ map (fromIntegral . length . fst . span (== 0)) $ chunksOf 8 $ bits ra
  vclzh ra = unshorts $ map (fromIntegral . length . fst . span (== 0)) $ chunksOf 16 $ bits ra
  vclzw ra = unwrds $ map (fromIntegral . length . fst . span (== 0)) $ chunksOf 32 $ bits ra
  vclzd ra = undwrds $ map (fromIntegral . length . fst . span (== 0)) $ chunksOf 64 $ bits ra
  vclzdm ra rb = vclzd (vand ra rb )

  {- Vector Merge Instructions -}

  vmrghb va vb = unbytes $ merge (first_halve(bytes va)) (first_halve(bytes vb))
  vmrglb va vb = unbytes $ merge (second_halve(bytes va)) (second_halve(bytes vb))

  vmrghh va vb = unshorts $ merge (first_halve(shorts va)) (first_halve(shorts vb))
  vmrglh va vb = unshorts $ merge (second_halve(shorts va)) (second_halve(shorts vb))

  vmrghw va vb = unwrds $ merge (first_halve(wrds va)) (first_halve(wrds vb))
  vmrglw va vb = unwrds $ merge (second_halve(wrds va)) (second_halve(wrds vb))

  vmrgew va vb = unwrds $ mergeEven (wrds va) (wrds vb)
  vmrgow va vb = unwrds $ mergeOdd (wrds va) (wrds vb)

  {- Vector Floating Point Arithmetic Instructions -}
  xvmaddmdp   = doubleOp3 (\ a b c -> a*b+c)
  xvmsubmdp   = doubleOp3 (\ a b c -> a*b-c)
  xvnmaddmdp   = doubleOp3 (\ a b c -> -(a*b+c))
  xvnmsubmdp   = doubleOp3 (\ a b c -> -a*b+c)
  vor     = dwrdOp2 (.|.)
  vorc    = dwrdOp2 (\ x y -> x .|. Bits.complement y)
  vxor    = dwrdOp2 Bits.xor
  vxorc   = dwrdOp2 (\ x y -> x `xor` Bits.complement y)
  vand    = dwrdOp2 (.&.)
  vandc   = dwrdOp2 (\ x y -> x .&. Bits.complement y)
  xvadddp    = doubleOp2 (+)
  xvsubdp    = doubleOp2 (-)
  xvmuldp    = doubleOp2 (*)

  dwrdOp2 f x y = undwrds $ zipWith f (dwrds x) (dwrds y)
  doubleOp2 f x y = unafdoubles $ zipWith f (afdoubles x) (afdoubles y)
  doubleOp3 f x y z = unafdoubles $ zipWith3 f (afdoubles x) (afdoubles y) (afdoubles z)
  unafdoubles = InterpVR . (\ [a,b] -> (a,b)) . map (fromIntegral . af2DVal)
  afdoubles = map (dval2af . fromIntegral) . (\ (a,b) -> [a,b]) . runInterpVR

  xvaddsp    = floatOp2 (+)
  xvsubsp    = floatOp2 (-)
  xvmulsp    = floatOp2 (*)
  xvmaddmsp   = floatOp3 (\ a b c -> a*b+c)
  xvnmaddmsp  = floatOp3 (\ a b c -> -(a*b+c))
  xvmsubmsp   = floatOp3 (\ a b c -> a*b-c)
  xvnmsubmsp  = floatOp3 (\ a b c -> -a*b+c)


  floatOp2 f x y = unfloats $ zipWith f (floats x) (floats y)
  floatOp3 f x y z = unfloats $ zipWith3 f (floats x) (floats y) (floats z)

{- Functions from Old PowerInterp.lhs-}
  -- xvcvdpsp xb = c2flt (sign 32) xb

{- Functions to replace old instructions -}
-- VSX Vector Convert with round
-- Double-Precision to Single-Precision format
-- XX2-form

  -- FIXME: Missing instruction
  xvcvdpsp xb = unfloats $ concatMap (\x -> [x,x]) $ doubles xb

  vrlqmi vt va vb = 
    let 
      list = bytes vb
      b = (list !! 5) .&. 0x7F
      e = (list !! 6) .&. 0x7F
      n = (list !! 7) .&. 0x7F
      r = Bits.rotateL (integer va) (fromIntegral n)
      m = mask128 b e
    in uninteger $ (r .&. m) .|. ((integer vt) .&. (mask128 e b))



  -- Compare a vector and an unsigned byte (vb)
  -- vcmpgtub va vb = 
  --   let a = floats va
  --       b = bytes vb
  --   in 
  --     if a >= b then unwrds $ replicate 4 0xFFFFFFFF
  --               else unwrds $ replicate 4 0

  -- compare two vectors >=
  vcmpgtfp va vb = 
    let a = floats va
        b = floats vb
    in 
      if a >= b then unwrds $ replicate 4 0xFFFFFFFF
                else unwrds $ replicate 4 0