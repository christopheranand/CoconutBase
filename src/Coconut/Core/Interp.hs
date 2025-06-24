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



module Coconut.Core.Interp where

import Data.Word (Word64, Word32, Word16, Word8)
import qualified Data.List as List
import qualified Data.ByteString as BS
import GHC.Arr (inRange)
import GHC.TypeLits
import Data.Proxy (Proxy(..))
import Numeric (showHex)
import Data.List (intercalate)

import Coconut.BaseTypes
import Coconut.Core.CoreISA
import Coconut.Utils.RunTimeSized
import Data.Int (Int64, Int8, Int16, Int32)
import Coconut.Utils.ArbFloat (dbl2Word32, dbl2Word64, chop, word322Dbl, word642Dbl, saturate, FPClass(..), af2DVal, dval2af)

import Data.Bits as Bits (xor, rotateL, complement, shift, shiftR, shiftL, (.&.), rotate, (.|.))
import Data.List.Split (chunksOf)

import Coconut.Utils.ArbFloat

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks k xs = let (ys, zs) = splitAt k xs in ys : chunks k zs

pupd f g (x,y) = (f x, g y)
pupdD f = pupd f f

padTruncateTo k x xs = take k $ xs ++ repeat x

nat :: forall n. (KnownNat n) => Int
nat = fromIntegral $ natVal (Proxy :: Proxy n)

safeDivMod x y = if y == 0 then error "divide by zero in safeDivMod" else divMod x y

-- | Interpretation instance for CoreISA (provides a haskell based interpretation of the DSL)
instance CoreISA Interp where
{-
 Getting values into GPRs
-}
  unintegerG  = InterpGPR . fromIntegral
  unwrd64     = InterpGPR
  unwrdsG     = InterpGPR . sum . zipWith (*) [2^(32*i) | i <- reverse [0.. 1]]
                          . map fromIntegral . padTruncateTo 2 0

  unwrd64CR   = InterpCR
  unintegerCR = InterpCR . fromIntegral
  gprToCR (InterpGPR reg) = InterpCR reg

  cmpGPRs g0 g1 = InterpCR $ if g0 == g1 then 1 else 0
  cmpVRs v0 v1 = InterpCR $ if v0 == v1 then 1 else 0

  uninteger   = InterpVR . pupdD fromIntegral . flip safeDivMod (2^64)
  unintegerS x = if x < 0
                 then uninteger (2^128 - abs x `mod` 2^128)
                 else uninteger (x `mod` 2^127)

  undwrds     = InterpVR . (\ [x,y] -> (x,y)) . map fromIntegral . padTruncateTo 2 0
  unwrds      = InterpVR . pupdD (sum . zipWith (*) [2^(32*i) | i <- reverse [0.. 1]])
                    . splitAt  2 . map fromIntegral . padTruncateTo 4 0
  unshorts    = InterpVR . pupdD (sum . zipWith (*) [2^(16*i) | i <- reverse [0.. 3]])
                    . splitAt  4 . map ((`mod` (2^16)) . fromIntegral) . padTruncateTo 8 0
  unbytes     = InterpVR . pupdD (sum . zipWith (*) [2^(8*i) | i <- reverse [0.. 7]])
                    . splitAt  8 . map ((`mod` (2^8)) . fromIntegral) . padTruncateTo 16 0
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
{-

-}
  signedG   = fromIntegral . runInterpGPR
  unsignedG x   = fromIntegral (fromIntegral $ runInterpGPR x :: Int64)
  wrd64       = fromIntegral . runInterpGPR
  wrdsG       = map fromIntegral . (\ (a,b) -> [a,b]) . flip divMod (2^32). runInterpGPR
{-

-}
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

  -- strings x = 
  --   let strVal = toEnum
  --   in strVal

  {- Spilling -}
  spillVR = InterpSPILL . runInterpVR
  despillVR = InterpVR . runInterpSPILL
  spillGPR = InterpSPILL . (,0) . runInterpGPR
  despillGPR = InterpGPR . fst . runInterpSPILL 


  {- Move Instructions -}
  movVR x = x
  movGPR x = x

  -- Memory regions will be aligned to quadword boundary, and must be a muliple of quadwords.
  -- In simpler terms, size would be the length of the VR list
  initMR :: Integer -> String -> [Interp VR] -> Interp MR
  initMR size name vals =
    let
      size' = fromIntegral $ size * 16
    in
      if fromIntegral size < length vals
      then error $ "ZInterp.initMR " ++ name ++ " size,#values " ++ take 300 (show (size', length vals))
      else
        let bs = BS.pack $ padTruncateTo size' 0 $ concatMap bytes vals
        in InterpMR $ PIMR name bs bs [] (fromIntegral size')

  incMR mr g = case mr of
    InterpMR (PIMR name vals atMeet ops size) ->
      let d = fromIntegral $ signedG g
          name' = name ++ "_" ++ show d
          vals' = BS.drop d vals
          size' = size- fromIntegral d
      in InterpMR (PIMR name' vals' BS.empty [] size')

  incMR0 mr d = case mr of
    InterpMR (PIMR name vals atMeet ops size) ->
      let name' = name ++ "_" ++ show d
          vals' = BS.drop d vals
          size' = size- fromIntegral d
      in InterpMR (PIMR name' vals' BS.empty [] size')
  {-case mr of
    InterpMR (PIMR name vals atMeet ops size) ->
      let idx = fromIntegral $ unG x
          idx' = (idx,idx+7)
          gprbytes = BS.pack $ chop 8 $ unG g
      in if all (inRange (0,(BS.length vals)-1)) [idx,idx+7]
        then InterpMR $ PIMR name (spliceByteString gprbytes vals idx') atMeet ((Store idx' gprbytes):ops) size
        else error $ "ZInterp.stdMR index out of range " ++ (take 300 $ show (idx,name,vals))-}

  {- Modulo Store/Load -}

  moduloDWStore mr0 mr d g = case (mr0,mr) of
    (InterpMR (PIMR name0 vals0 atMeet0 ops0 size0),InterpMR (PIMR name vals atMeet ops size)) ->
      let idx = d + fromIntegral (size0-size)
          idx' = (idx, idx+7)
          gprbytes = BS.pack $ bytesG g
      in if all (inRange (0,BS.length vals0-1)) [idx,idx+7]
         then InterpMR $ PIMR name0 (spliceByteString gprbytes vals0 idx') atMeet0 (Store idx' gprbytes:ops0) size0
         --then InterpMR $ PIMR name0 (spliceByteString vrbytes vals0 idx') atMeet0 (Store idx' vrbytes:ops0) size0
         else error $ "Core.Interp.moduloDWStore index out of range " ++ take 300 (show (idx,name0,vals0))

  moduloDWLoad mr0 mr d = case (mr0,mr) of
    (InterpMR (PIMR name0 vals0 atMeet0 ops0 size0),InterpMR (PIMR name vals atMeet ops size)) ->
      let idx = d + fromIntegral (size0-size)
          idx' = (idx,idx+7)
      in if all (inRange (0,BS.length vals0-1)) [idx,idx+7]
         then (unintegerG $ fromIntegral $ bytes2Dwrd $ BS.unpack $ BS.take 8 $ BS.drop idx vals0,
              InterpMR $ PIMR name0 vals0 atMeet0 (Load idx':ops0) size0)
        -- then (undwrds $ map bytes2Dwrd $ chunks 4 $ BS.unpack $ BS.take 8 $ BS.drop idx vals0,
        --       InterpMR $ PIMR name0 vals0 atMeet0 (Load idx':ops0) size0)
         else error $ "ZInterp.moduloDWLoad index out of range. d: " ++ show d
                   ++ " length vals: " ++ show (BS.length vals)

  moduloVStore mr0 mr d v = case (mr0,mr) of
    (InterpMR (PIMR name0 vals0 atMeet0 ops0 size0),InterpMR (PIMR name vals atMeet ops size)) ->
      let idx = d + fromIntegral (size0-size)
          idx' = (idx, idx+15)
          vrbytes = BS.pack $ uncurry (<>) $ pupdD (chop 8) $ runInterpVR v
      in if all (inRange (0,BS.length vals0-1)) [idx,idx+15]
         then InterpMR $ PIMR name0 (spliceByteString vrbytes vals0 idx') atMeet0 (Store idx' vrbytes:ops0) size0
         else error $ "Core.Interp.moduloVStore index out of range " ++ take 300 (show (idx,name0,vals0))

  moduloVLoad mr0 mr d = case (mr0,mr) of
    (InterpMR (PIMR name0 vals0 atMeet0 ops0 size0),InterpMR (PIMR name vals atMeet ops size)) ->
      let idx = d + fromIntegral (size0-size)
          idx' = (idx,idx+15)
      in if all (inRange (0,BS.length vals0-1)) [idx,idx+15]
        then (undwrds $ map bytes2Dwrd $ chunks 8 $ BS.unpack $ BS.take 16 $ BS.drop idx vals0,
              InterpMR $ PIMR name0 vals0 atMeet0 (Load idx':ops0) size0)
        else error $ "ZInterp.moduloVLoad index out of range. d: " ++ show d
                   ++ " length vals: " ++ show (BS.length vals)

  moduloVStoreG mr d x v = case mr of
    InterpMR (PIMR name vals atMeet ops size) ->
      let idx = (fromIntegral $ runInterpGPR x) + d
          idx' = (idx, idx+15)
          vrbytes = BS.pack $ uncurry (<>) $ pupdD (chop 8) $ runInterpVR v
      in if all (inRange (0,BS.length vals-1)) [idx,idx+15]
         then InterpMR $ PIMR name (spliceByteString vrbytes vals idx') atMeet (Store idx' vrbytes:ops) size
         else error $ "Core.Interp.moduloVLoadG index out of range " ++ take 300 (show (idx,name,vals))

  moduloVLoadG mr d x = case mr of
    InterpMR (PIMR name vals atMeet ops size) ->
      let idx = fromIntegral $ d + fromIntegral (signedG x)
          idx' = (idx,idx+15)
      in if all (inRange (0,BS.length vals-1)) [idx,idx+15]
        then (undwrds $ map bytes2Dwrd $ chunks 8 $ BS.unpack $ BS.take 16 $ BS.drop idx vals,
              InterpMR $ PIMR name vals atMeet (Load idx':ops) size)
        else error $ "Core.Interp.moduloVLoadG index out of range. d: " ++ show d ++ " x: " ++ show (signedG x)
                   ++ " length vals: " ++ show (BS.length vals)

  -- scratchStoreVR mr d v = case mr of
  --   InterpMR (PIMR name vals atMeet ops size) ->
  --     let idx = d
  --         idx' = (idx, idx+15)
  --         vrbytes = BS.pack $ uncurry (<>) $ pupdD (chop 8) $ runInterpVR v
  --     in if all (inRange (0,BS.length vals-1)) [idx,idx+15]
  --        then InterpMR $ PIMR name (spliceByteString vrbytes vals idx') atMeet (Store idx' vrbytes:ops) size
  --        else error $ "Core.Interp.moduloVLoadG index out of range " ++ take 300 (show (idx,name,vals))

  -- scratchLoadVR mr d = case mr of
  --   InterpMR (PIMR name vals atMeet ops size) ->
  --     let idx = fromIntegral $ d
  --         idx' = (idx,idx+15)
  --     in if all (inRange (0,BS.length vals-1)) [idx,idx+15]
  --       then (undwrds $ map bytes2Dwrd $ chunks 8 $ BS.unpack $ BS.take 16 $ BS.drop idx vals,
  --             InterpMR $ PIMR name vals atMeet (Load idx':ops) size)
  --       else error $ "Core.Interp.moduloVLoadG index out of range. d: " ++ show d
  --                  ++ " length vals: " ++ show (BS.length vals)

  {- Generic logical/arithmetic instructions
   -}
  orG ra rb = InterpGPR $ runInterpGPR ra .|. runInterpGPR rb
  xorG ra rb = InterpGPR $ runInterpGPR ra `Bits.xor` runInterpGPR rb
  andG ra rb = InterpGPR $ runInterpGPR ra .&. runInterpGPR rb
  addG ra rb = InterpGPR $ runInterpGPR ra + runInterpGPR rb
  subG ra rb = InterpGPR $ runInterpGPR ra - runInterpGPR rb

  {- Generic branching instructions
   -}
  jump = InterpBRANCH True
  branchNotLow r0 r1 = InterpBRANCH $ runInterpGPR r0 >= runInterpGPR r1
  branchLow r0 r1 = InterpBRANCH $ runInterpGPR r0 < runInterpGPR r1
  branchImmNotEq r0 imm = InterpBRANCH $ runInterpGPR r0 /= fromIntegral imm
  branchImmNotHigh r0 imm = InterpBRANCH $ runInterpGPR r0 <= fromIntegral imm

-- Helper functions
rotateLeft, rotateLeft32 :: Word64 -> Word64 -> Word64
rotateLeft ra rb = shiftL ra (fromIntegral rb) .&. (2^64 - 1) .|. shiftR ra (64-fromIntegral rb)
rotateLeft32 ra' rb = let ra = ra' .&. (2^32 - 1)
  in shiftL ra (fromIntegral rb) .&. (2^32 - 1) .|. shiftR ra (32-fromIntegral rb)


{-
Rotate/shift combine logical instruction. Control argument is defined as follows (little endian):
bit 42: shift or rotate switch
bits 41-35: biased shift amount (shift left by x-2^6+1)
bits 34-40: segment 1 flags
bits 29-25: segment 2 flags
bits 24-20: segment 3 flags
bits 19-15: segment 4 flags
bits 14-10: width of segment 1
bits 9-5: width of segment 2
bits 4-0: width of segment 3

Each segment flags group is laid out as follows (little endian):
bit 4: If 1, use A, otherwise replace A with all zeros
bit 3: If 1, use B, otherwise replace B with all zeros
bit 2: If 1, replace A with ~A (after the replacement in bit 4)
bit 1: If 1, replcae B with ~B (after the replacement in bit 3)
bit 0: If 1, the instruction will perform an OR operation instead of an AND in this segment

%\begin{code}
  qvrscl a b c =
    undwrds $ zipWith3 qvrsclScalar (dwrds a) (dwrds b) (dwrds c)
    where
      qvrsclScalar :: Integer -> Integer -> Integer -> Integer
      qvrsclScalar a b c =
        subResult
        where
          decodeSeg x = RSCLSegOptions (testBit x 4) (testBit x 3) (testBit x 2) (testBit x 1) (testBit x 0)
          a' = shiftRot a amount
          shiftRot = if testBit c 42 then shift else rotate
          amount = fromEnum $ ((c `shiftR` (42-7)) .&. (2^7-1)) - 2^6 + 1
          lengths = map (\ i -> (c `shiftR` (5*(2-i))) .&. 0x1f :: Integer) [0..2]
          lengths' = lengths ++ [max (64 - (sum lengths)) 0]
          segOpts = map (\ i -> decodeSeg ((c `shiftR` (15+5*(3-i))) .&. 0x1f :: Integer)) [0..3]
          segMasks = take 4 ((snd $ mapAccumL (\i x -> (i+x,(2^(64-i)-1) - (2^(64-i-x)-1) :: Integer)) 0 lengths') ++ (repeat 0))
          segOpers = map (\ x -> if isOr x then (.|.) else (.&.)) segOpts
          segsA = map (\ x -> let za = (if useA x then a' else 0) in if complA x then complement za else za) segOpts
          segsB = map (\ x -> let zb = (if useB x then b else 0) in if complB x then complement zb else zb) segOpts
          segsCombined = zipWith3 (\f a b -> f a b) segOpers segsA segsB

          subResult = foldr1 (.|.) (map (\ i -> (segsCombined !! i) .&. (segMasks !! i)) [0..3])
%\end{code}
%

Vector logicals
-}

  -- TODO finish implementing CoreISA Interp


zipNoCC :: (Num a, Integral b) => ([b] -> Interp VR) -> (a -> a -> Bool) -> [a] -> [a] -> Interp VR
zipNoCC mkVR test as bs = mkVR $ zipWith (\ a b -> mkOnes $ test a b) as bs
--zipCC :: (Num a, Integral b) => ([b] -> Interp VR) -> (a -> a -> Bool) -> [a] -> [a] -> (Interp VR, CR Interp)
--zipCC _ _ _ _ = error $ "ZInterp vector integer compares with CC not yet implemented"
mkOnes True = fromIntegral (-1 :: Integer)
mkOnes False = 0
{-

-}
sum4s (a:b:c:d:rest) = (a+b+c+d) : sum4s rest
sum4s _ = []
by4s (_:_:_:d:rest) = d : by4s rest
by4s _ = []
sum2s (c:d:rest) = (c+d) : sum2s rest
sum2s _ = []
by2s (_:d:rest) = d : by2s rest
by2s _ = []
{-

-}
mergeHi [] [] = []
mergeHi (a1:_:as) (b1:_:bs) = a1:b1:mergeHi as bs
mergeHi a b = error $ "ZInterp.mergeHi illegal "++show(a,b)
mergeLo [] [] = []
mergeLo (_:a1:as) (_:b1:bs) = a1:b1:mergeLo as bs
mergeLo a b = error $ "ZInterp.mergeLo illegal "++show(a,b)
saturate max x
  | x >= max = max-1
  | x < -max = -max
  | otherwise = x
saturateLogical max x = if x >= max || x < 0 then max-1 else x
{-

-}

{-

Convert double to Interp GPR
FIXME:  get this problem for bytes and shorts
*SqrtZ> bytes $ verim 0 1 (unshorts8 1) (unshorts8 1::Interp VR) (unshorts8 $ 2*0xf)
[*** Exception: divide by zero

-}
zw3 sh l1 l2 l3 = l1 .&. Bits.complement l3 .|. Bits.rotate l2 sh .&. l3
double2GPR :: Double -> Interp GPR
double2GPR x =  InterpGPR . fromIntegral $ (integer @Interp) . undoubles $ [0,x]
{-

constants for +ve Inf, -ve Inf, NaN

-}
pInfG, nInfG, pNaNG, nNaNG, zeroG :: Interp GPR
pInfG = unwrd64 0x7ff0000000000000
nInfG = unwrd64 0xfff0000000000000
pNaNG = unwrd64 0x7ff8000000000000
nNaNG = unwrd64 0xfff8000000000000
zeroG = unwrd64 0x0000000000000000
{-

Apply scalar single-precision floating point operations
-}
unafdoubles = InterpVR . (\ [a,b] -> (a,b)) . map (fromIntegral . af2DVal)
afdoubles = map (dval2af . fromIntegral) . (\ (a,b) -> [a,b]) . runInterpVR
{-

-}
doubleOp f = unafdoubles . map f . afdoubles
doubleOp2 f x y = unafdoubles $ zipWith f (afdoubles x) (afdoubles y)
doubleOp3 f x y z = unafdoubles $ zipWith3 f (afdoubles x) (afdoubles y) (afdoubles z)
{-

-}
dwrdOp :: (Word64 -> Word64) -> Interp VR -> Interp VR
dwrdOp2 :: (Word64 -> Word64 -> Word64) -> Interp VR -> Interp VR -> Interp VR
dwrdOp3 :: (Word64 -> Word64 -> Word64 -> Word64) -> Interp VR -> Interp VR -> Interp VR -> Interp VR
dwrdOp f = undwrds . map f . dwrds
dwrdOp2 f x y = undwrds $ zipWith f (dwrds x) (dwrds y)
dwrdOp3 f x y z = undwrds $ zipWith3 f (dwrds x) (dwrds y) (dwrds z)
{-

-}
fTest :: [FPClass] -> Word64 -> Word64
fTest (c:cs) x = fTest' c x .|. fTest cs x
fTest [] _ = 0
fTest' c x | x == 0x7ff0000000000000
           = if c==FPInf then yes64 else 0
fTest' c x | x == 0xfff0000000000000
           = if c==FPNegInf then yes64 else 0
fTest' c x | x == 0
           = if c==FPZero then yes64 else 0
fTest' c x | x == 0x8000000000000000
           = if c==FPNegZero then yes64 else 0
fTest' c x | 0 == x .&. 0x7ff0000000000000
           = if c==FPSubnormal && 0 == x .&. 0x8000000000000000 ||
                c==FPNegSubnormal && 0 /= x .&. 0x8000000000000000
             then yes64 else 0
fTest' c x | x == 0x7ff8000000000000
           = if c==FPQNaN then yes64 else 0
fTest' c x | x == 0xfff8000000000000
           = if c==FPNegQNaN then yes64 else 0
fTest' c x | x .&. 0xfff0000000000000 == 0x7ff0000000000000
           = if c==FPSNaN then yes64 else 0
fTest' c x | x .&. 0xfff0000000000000 == 0xfff0000000000000
           = if c==FPNegSNaN then yes64 else 0
fTest' c x | x .&. 0x8000000000000000 == 0
           = if c==FPNormal then yes64 else 0
fTest' c _ = if c==FPNegNormal then yes64 else 0

yes64 = 0xffffffffffffffff :: Word64
{-

-}
idxh, idxw, idxd :: Integer -> Integer -> Integer -> Integer
idxh addr idx x | wrongElem   = x
                | evenByte    = 3
                | otherwise   = 2
   where evenByte = 0 == 1 .&. idx
         wrongElem = (addr - idx) `div` 2  /=  0

idxw addr idx x | wrongElem   = x
                | otherwise   = byte
   where byte = 3 .&. idx
         wrongElem = (idx - addr) `div` 4  /=  0

idxd addr idx x | wrongElem   = x
                | otherwise   = 3 - byte
   where byte = 7 .&. idx
         wrongElem = (addr - idx) `div` 8  /=  0
{-


-}
perm lst c = if 0x4000000000000000 == 0x7ff0000000000000 .&. c
                then lst !! fromEnum (shiftR (c :: Word64) 49 .&. 7)
                else 0xffffffffffffffff -- return a bad NaN for undefined so we know we detect use

{-

Byte and ByteString utilities, used for MemRegions
-}
-- vector register -> 32 bytes

bytesG :: Interp GPR -> [Word8]
bytesG = map fromIntegral . chop 8 . runInterpGPR

-- 8 byte list -> 64 byte dword
bytes2Dwrd :: [Word8] -> Word64
bytes2Dwrd xs = sum (zipWith (\ x y -> fromIntegral y * 2^(8*x)) [7,6..0] xs)
bytes2Wrd :: [Word8] -> Word32
bytes2Wrd xs = sum (zipWith (\ x y -> fromIntegral y * 2^(8*x)) [3,2,1,0] xs)

{-

-}
safeRecip :: Double -> Double
safeRecip x = Coconut.Utils.ArbFloat.saturate $ 1/x
{-

Round to integer.  \cka{Since |round| seems to round to nearest ties to even, these are all wrong.
When there is time, switch to |ArbFloat|.}
-}
roundToZero x = if Prelude.isNaN x then x else fromIntegral $ truncate x
roundNearest x = if Prelude.isNaN x then x else fromIntegral $ round x
roundUp x = if Prelude.isNaN x then x else fromIntegral $ ceiling x
roundDown x = if Prelude.isNaN x then x else fromIntegral $ floor x
{-

|sExt| maps the positive integer |imm|
representing a signed |bitsIn|-bit integer $k$
into the positive integer
representing $k$ as a signed |bitsOut|-bit integer.
Any higher bits in |imm| (beyond |bitsIn|, i.e., if |imm >= 2^bitsIn|)
are ignored.

-}
sExt :: Integer -> Integer -> Integer -> Integer
sExt bitsIn bitsOut imm
  | bitsOut < bitsIn || bitsIn < 1    = error $ "ZInterp.sExt sizes "++show (bitsIn,bitsOut)
  | highBitVal .&. imm == highBitVal  = 2^bitsOut - bitsInVal .|. bitsInMasked_imm
  | otherwise                         = bitsInMasked_imm
  where
    highBitPos = pred bitsIn
    highBitVal = 2 ^ highBitPos
    bitsInVal = highBitVal `shiftL` 1  -- = 2 ^ bitsIn|
    bitsInMask = pred bitsInVal
    bitsInMasked_imm = bitsInMask .&. imm

s7, s10, s10to16 :: Integer -> Integer

s10to16  = sExt 10  16
s10      = sExt 10  32
s7       = sExt 7   32
{-

\edcomm{CKA}{I think this should be equality.}

\edcomm{WK}{I guess |sign| converts positive integers
considered as ``unsinged'' integer representations
into the integer that results from interpreting this bit pattern
as a ``signed'' integer representation in |b| bits.

Perhaps this should be moved into |PPCAltivecConstants|
to be together with |unsign| again,
or even together into a general bit-utility module.
}

\begin{spec}
sign b (unsign b x) = x
unsign b (sign b x) = x
\end{spec}

-}
sign b v  | b < 1         = error $ "ZInterp.sign neg bit "++show b
          | v >= 2^(b-1)  = v - 2^b
          | otherwise     = v
{-

-}
cmpu from to cmp ra rb = to $ zipWith (\ a b -> if cmp a b then -1 else 0) (from ra) (from rb)
cmpui from to cmp ra imm = to $ map ((\ a b -> if cmp a b then -1 else 0) imm) (from ra)
cmps from to cmp ra rb = to $ zipWith (\ a b -> if cmp a b then -1 else 0) (from ra) (from rb)
cmpsi from to cmp ra imm = to $ map ((\ a b -> if cmp a b then -1 else 0) imm) (from ra)
{-

-}
--instance Taggable (Interp VR)
--instance Taggable (GPR INTERP)
--instance Taggable (CR INTERP)
{-
-}
