-- |
-- Module      :  Coconut.Utils.ArbFloat
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- Arbitrary precision floating point arithmetic following IEEE conventions.
-- Start with arithmetic, and adds functions later.

module Coconut.Utils.ArbFloat where

-- TODO document ArbFloat

import Data.Bits as Bits
import Data.Word as Unsigned

data FPClass = FPZero | FPNormal | FPSubnormal | FPInf | FPQNaN | FPSNaN
             | FPNegZero | FPNegNormal | FPNegSubnormal | FPNegInf | FPNegQNaN | FPNegSNaN
  deriving (Read,Show,Eq,Ord)
fpT FPZero = 1
fpT FPNegZero = 2
fpT FPNormal = 4
fpT FPNegNormal = 8
fpT FPSubnormal = 16
fpT FPNegSubnormal = 32
fpT FPInf = 64
fpT FPNegInf = 128
fpT FPQNaN = 256
fpT FPNegQNaN = 512
fpT FPSNaN = 1024
fpT FPNegSNaN = 2048
fpTestImm fp = sum $ map fpT fp
fpTestClasses :: Unsigned.Word64 -> [FPClass]
fpTestClasses x = concat
    [[FPZero | x Bits..&. 1 /= 0]
    ,[FPNegZero | x Bits..&. 2 /= 0]
    ,[FPNormal | x Bits..&. 4 /= 0]
    ,[FPNegNormal | x Bits..&. 8 /= 0]
    ,[FPSubnormal | x Bits..&. 16 /= 0]
    ,[FPNegSubnormal | x Bits..&. 32 /= 0]
    ,[FPInf | x Bits..&. 64 /= 0]
    ,[FPNegInf | x Bits..&. 128 /= 0]
    ,[FPQNaN | x Bits..&. 256 /= 0]
    ,[FPNegQNaN | x Bits..&. 512 /= 0]
    ,[FPSNaN | x Bits..&. 1024 /= 0]
    ,[FPNegSNaN | x Bits..&. 2048 /= 0]
    ]
data FPRounding = FPDefault | FPNearestAway0 | FP2Flt | FPNearestToEven | FPTo0 | FPToInf | FPToNegInf
  deriving (Read,Show,Eq,Ord)
fpImm FPDefault = 0
fpImm FPNearestAway0 = 1
fpImm FP2Flt = 3
fpImm FPNearestToEven = 4
fpImm FPTo0 = 5
fpImm FPToInf = 6
fpImm FPToNegInf = 7
immFp 0 = FPDefault
immFp 1 = FPNearestAway0
immFp 3 = FP2Flt
immFp 4 = FPNearestToEven
immFp 5 = FPTo0
immFp 6 = FPToInf
immFp 7 = FPToNegInf
immFp x = error $ "illegal value for immFp "++show x
{-

-}
data ArbFloat = NaN | PInf | MInf
  | AF  {afExp :: Integer
        ,afSig :: Integer
        ,afSign :: Integer
        ,afBase :: Integer
        }
     deriving (Show)
{-

-}
instance Num ArbFloat where
  abs (AF exp sig sign base) = AF exp sig 1 base
  abs PInf = PInf
  abs MInf = PInf
  abs NaN = NaN

  negate (AF exp sig sign base) = AF exp sig (-sign) base
  negate PInf = MInf
  negate MInf = PInf
  negate NaN = NaN

  signum (AF exp sig sign base) = AF 0 1 sign base
  signum PInf = AF 0 1 1 2
  signum MInf = AF 0 1 (-1) 2
  signum NaN = NaN

  fromInteger x = if x >= 0 then AF 0 x 1 2 else AF 0 (-x) (-1) 2

  (+) NaN _ = NaN
  (+) _ NaN = NaN
  (+) PInf MInf = NaN
  (+) MInf PInf = NaN
  (+) MInf _ = MInf
  (+) PInf _ = PInf
  (+) _ MInf = MInf
  (+) _ PInf = PInf
  (+) x1@(AF exp1 sig1 sign1 2) x2@(AF exp2 sig2 sign2 2)
     = AF (afExp small) sig sign 2
       where
         (big,small) = if exp1 > exp2 then (x1,x2) else (x2,x1)
         sig' = afSig big * afSign big * twoPow (afExp big - afExp small) + afSig small * afSign small
         (sig,sign) = if sig' >= 0 then (sig',1) else (-sig',-1)
  (+) _ _ = error "ArbFloat only implments base 2"

  (*) NaN _ = NaN
  (*) _ NaN = NaN
  (*) PInf MInf = MInf
  (*) MInf PInf = MInf
  (*) PInf PInf = PInf
  (*) MInf MInf = PInf
  (*) MInf (AF _ 0 _ _) = NaN
  (*) PInf (AF _ 0 _ _) = NaN
  (*) MInf (AF _ _ sign _) = if sign == 1 then MInf else PInf
  (*) PInf (AF _ _ sign _) = if sign == 1 then PInf else MInf
  (*) (AF _ 0 _ _) MInf = NaN
  (*) (AF _ 0 _ _) PInf = NaN
  (*) (AF _ _ sign _) MInf = if sign == 1 then MInf else PInf
  (*) (AF _ _ sign _) PInf = if sign == 1 then PInf else MInf
  (*) x1@(AF exp1 sig1 sign1 2) x2@(AF exp2 sig2 sign2 2)
     = AF (exp1 + exp2) (sig1 * sig2) (sign1 * sign2) 2
  (*) _ _ = error "ArbFloat only implments base 2"
{-

-}
twoPow :: Integer -> Integer
twoPow 0 = 1
twoPow 1 = 2
twoPow 2 = 4
twoPow x = case divMod x 2 of
  (q,0) -> let x1 = twoPow q in x1 * x1
  (q,1) -> let x1 = twoPow q in x1 * x1 * 2
  _ -> error "twoPow: impossible"
{-

\edcomm{CKA}{Think about whether we should adjust exponents before compare.}
-}
instance Eq ArbFloat where
  (==) x1@(AF exp1 sig1 sign1 2) x2@(AF exp2 sig2 sign2 2)
    = sign1 * sig1 * e1 == sign2 * sig2 * e2
      where emin = min exp1 exp2
            e1 = 2 ^ (exp1 - emin)
            e2 = 2 ^ (exp2 - emin)
  (==) PInf PInf = True
  (==) MInf MInf = True
  (==) _ _ = False
{-

-}
lessThan NaN _ = False
lessThan _ NaN = False
lessThan _ MInf = False
lessThan MInf _ = True
lessThan PInf _ = False
lessThan _ PInf = True
lessThan x1@(AF exp1 sig1 sign1 2) x2@(AF exp2 sig2 sign2 2)
  = sign1 * sig1 * e1 < sign2 * sig2 * e2
    where emin = min exp1 exp2
          e1 = 2 ^ (exp1 - emin)
          e2 = 2 ^ (exp2 - emin)
lessThan a b = error $ "lessThan only supports base 2" ++ show (a,b)
{-

-}
greaterThan NaN _ = False
greaterThan _ NaN = False
greaterThan MInf _ = False
greaterThan _ MInf = True
greaterThan _ PInf = False
greaterThan PInf _ = True
greaterThan x1@(AF exp1 sig1 sign1 2) x2@(AF exp2 sig2 sign2 2)
  = sign1 * sig1 * e1 > sign2 * sig2 * e2
    where emin = min exp1 exp2
          e1 = 2 ^ (exp1 - emin)
          e2 = 2 ^ (exp2 - emin)
greaterThan a b = error $ "lessThan only supports base 2" ++ show (a,b)
{-

-}
greaterThanEqual NaN _ = False
greaterThanEqual _ NaN = False
greaterThanEqual _ MInf = True
greaterThanEqual PInf _ = True
greaterThanEqual x1@(AF exp1 sig1 sign1 2) x2@(AF exp2 sig2 sign2 2)
  = sign1 * sig1 * e1 >= sign2 * sig2 * e2
    where emin = min exp1 exp2
          e1 = 2 ^ (exp1 - emin)
          e2 = 2 ^ (exp2 - emin)
greaterThanEqual a b = error $ "lessThan only supports base 2" ++ show (a,b)
{-


Round to specified number of bits.

Keep rounding
-}
afRound bits (AF exp sig sign 2) =
  let  (sig',shift) = roundDown (2^bits,1,0,0) sig
       (sig'',shift') = boostUp (2^(bits-1),shift) sig'
  in if bits < 1 then error $ " ArbFloat.afRound bits " ++ show bits
     else AF (exp+shift') sig'' sign 2
afRound _ AF {} = error "afRound supports base 2"
afRound _ x = x

roundDown (topBit,lastBit,roundBit,bitShift) x
  = if topBit <= x + roundBit then roundDown (2*topBit,2*lastBit,lastBit,bitShift+1) x
    else (divErr (x + roundBit) lastBit,bitShift)
  where
    divErr x y = if y == 0
                    then error "divide by zero in roundDown"
                    else x `div` y
boostUp (topBit,bitShift) x
  | x == 0 = (0,0)
  | topBit > x = boostUp (topBit,bitShift-1) (2*x)
  | otherwise = (x,bitShift)
{-

Convert to/from integer
-}
af2s64 _ NaN  = 0
af2s64 _ PInf = 0x7fffffffffffffff
af2s64 _ MInf = 0x8000000000000000
af2s64 mode (AF exp sig sign 2) =
  let
    divModErr x y = if y == 0
                       then error "divMod by zero in af2s64"
                       else x `divMod` y
    signed = if exp > 0 then sign * sig * twoPow exp
                        else let pow2 = twoPow (-exp)
                                 (intPart,fracPart) = divModErr sig pow2
                                 intEven = if odd intPart then intPart+1 else intPart
                             in case (sign,mode) of
                                  (_,FPDefault) -> error "FPDefault not supported"
                                  (_,FPNearestAway0) -> case (2*fracPart) `compare` pow2 of
                                                        EQ -> sign*(intPart+1)
                                                        LT -> sign*intPart
                                                        GT -> sign*(intPart+1)
                                  (_,FP2Flt) -> error "AF.af2s64.FP2Flt"
                                  (_,FPNearestToEven) -> case (2*fracPart) `compare` pow2 of
                                                        EQ -> sign*intEven
                                                        LT -> sign*intPart
                                                        GT -> sign*(intPart+1)
                                  (_,FPTo0) -> sign*intPart
                                  (1,FPToInf) -> if fracPart > 0 then intPart+1 else intPart
                                  (-1,FPToInf) -> -intPart
                                  (1,FPToNegInf) -> intPart
                                  (-1,FPToNegInf) -> if fracPart > 0 then -intPart-1 else -intPart
                                  _ -> error $ "AF.af2s64 impossible "++show (sign,mode)
  in
    if signed >= 2^63 then 0x7fffffffffffffff
      else if signed < (-2^63) then 0x8000000000000000
      else if signed >= 0 then signed
      else 2^64 + signed
af2s64 _ _ = error "ArbFloat.af2s64 only implments base 2"

af2u64 _ NaN  = 0
af2u64 _ PInf = 0xffffffffffffffff
af2u64 _ MInf = 0x0000000000000000
af2u64 mode (AF exp sig sign 2) =
  let
    divModErr x y = if y == 0
                       then error "divMod by 0 in af2u64"
                       else x `divMod` y
    signed = if exp > 0 then sign * sig * twoPow exp
                        else let pow2 = twoPow (-exp)
                                 (intPart,fracPart) = divModErr sig pow2
                                 intEven = if odd intPart then intPart+1 else intPart
                             in case (sign,mode) of
                                  (-1,_) -> 0
                                  (1,FPDefault) -> error "FPDefault not supported"
                                  (1,FPNearestAway0) -> case (2*fracPart) `compare` pow2 of
                                                        EQ -> intPart+1
                                                        LT -> intPart
                                                        GT -> intPart+1
                                  (1,FP2Flt) -> error "AF.af2u64.FP2Flt"
                                  (1,FPNearestToEven) -> case (2*fracPart) `compare` pow2 of
                                                        EQ -> intEven
                                                        LT -> intPart
                                                        GT -> intPart+1
                                  (1,FPTo0) -> sign*intPart
                                  (1,FPToInf) -> if fracPart > 0 then intPart+1 else intPart
                                  (1,FPToNegInf) -> intPart
                                  _ -> error $ "AF.af2u64 impossible "++show (sign,mode)
  in
    if signed >= 2^64 then 0xffffffffffffffff
      else if signed < 0 then 0x0000000000000000
      else signed
af2u64 _ _ = error "ArbFloat.af2s64 only implments base 2"



{-
%}}}

%{{{ chop exp v
|chop exp v| produces a list of |exp|-bit values
together representing |v| as a 128-bit number.

-}

chop :: Integral a => Word64 -> Word64 -> [a]
chop exp x = reverse $ take (if exp == 0 then error $ "chop/" ++ show x else divErr 64 $ fromIntegral exp)
   $ chop' $ fromIntegral x -- chop0 64 exp v
  where
    divErr x y = if y == 0 then error "div by zero in chop" else x `div` y
    divModErr :: Integer -> Integer -> (Integer,Integer)
    divModErr x y = if y == 0
                    then error ("divMod by zero in chop " ++ show exp ++ " " ++ show x)
                    else x `divMod` y
    divisor :: Integer
    divisor = 2^(fromIntegral exp)
    chop' x = let (quo,rem) = divModErr x divisor in (fromIntegral rem) : chop' quo

digits = floatDigits (1::Double) - 1

word642Dbl :: Word64 -> Double
word642Dbl v = if exp' == 0x7ff
  then
    if mantissa' == 0
    then      --infinity
      if sign' == 1  then -1/0  --Neg infinity
                     else  1/0  --Pos Infinity
    else 0/0                    --NaN
  else sign * fromIntegral mantissa * expPart
  where
    (sign',v') = divMod v (2^63)
    (exp',mantissa') = divMod v' (2^52)
    sign = if sign' == 0 then 1 else -1
    exp = fromIntegral exp' - 1023
    (mantissa,expPart) = if exp' == 0
         then (mantissa',2**(-1022 - 52))
         else (2^52 + mantissa', 2 ** (exp - fromIntegral digits))

dbl2Word64 :: Double -> Word64
dbl2Word64 d
  | exp == 2047 =
      if mantissa == 0 then                      --Infinity
        if sign == 2^63 then 0xFFF0000000000000  --Neg Infinity
        else 0x7FF0000000000000                  --Pos infinity
      else 0xFFF1000000000000
        | (0,0) == (mantissa',exp') = sign
        | otherwise = sign .|. exp * 2^52 .|. fromIntegral mantissa
  where
      sign = if d < 0 then 2 ^ 63 else 0
      (mantissa', exp') = decodeFloat (signum d * d)
      texp = fromIntegral $ exp' + 1023 + digits
      (exp, mantissa)
        = if texp < 1 then
              (0, shiftR mantissa' ((- 1) * (exp' + 1022 + digits)))
          else
              (texp, mantissa' - 2 ^ digits)

word322Dbl :: Word32 -> Double
word322Dbl v
  | e' == 0 && m' == 0 = 0
  | e' == 255 = if m' == 0 then 1/0 * sign else 0/0
  | otherwise = sign * encodeFloat mantissa exp
  where
      (se', m') = divMod (fromIntegral v) (2 ^ 23)
      (s', e') = divMod se' (2 ^ 8)
      sign = if s' == 0 then 1 else - 1
      exp
        = if e' == 0 then
              - 126 - 23 - (23 - significantBits m' 1)
          else
              fromInteger $ e' - 127 - 23
      mantissa
        = if e' == 0 then
              shiftL m' (23 - significantBits m' 1)
          else
              m' + 2 ^ 23
      significantBits m n
        = if div m 2 /= 0 then significantBits (div m 2) (n + 1) else n

dbl2Word32 :: Double -> Word32
dbl2Word32 d' = if d' /= d' then 0x7fc00000 --NaN /= NaN
            else if d == 1/0 then 0x7f800000 else if d == (-1/0) then 0xff800000
          else if d == 0 then 0
          else sign
               .|. fromIntegral exp' * 2^23
               .|. fromIntegral mantissa
 where d =  realToFrac d' :: Float     -- make it a valid SPU float, does correct rounded in case of subnorms
       sign = if d < 0 then 2^31 else 0
       (m',e') = decodeFloat $ signum d * d
       exp' = if e' < -126-23 then 0 else e'+23+127
       mantissa = {-if e' == 0 then 0  -- I don't know why this condition is here
                  else -}if  e' < -(126+23) then shiftR m' (-126-23-e')
                  else m' - 2^23


{- convert ArbFloat to and from IEEE 64-bit binary rep
 -}
dval2af :: Integer -> ArbFloat
dval2af v = case (exp',mantissa',sign') of
    (2047,0,0) -> PInf
    (2047,0,1) -> MInf
    (2047,_,_) -> NaN
    (0,_,_) -> AF (exp' - 1022 - 52) mantissa' sign 2
    _ -> AF (exp' - 1023 - 52) mantissa sign 2
  where  (sign',v') = divMod v (2^63)
         (exp',mantissa') = divMod v' (2^52)
         sign = if sign' == 0 then 1 else -1
         mantissa = 2^52 + mantissa'

af2DVal :: ArbFloat -> Integer
af2DVal = af2DVal' . afRound 53

af2DVal' NaN = 2047 * 2^52 + 2^51
af2DVal' PInf = 2047 * 2^52
af2DVal' MInf = 2^63 + 2047 * 2^52
af2DVal' (AF _ 0 sign 2) = if sign == 1 then 0 else 2^63
af2DVal' (AF exp sig sign 2)
  = (if sign == 1 then 0 else 2^63)
    + unsigned exp sig
af2DVal' _ = error "af2DVal supports base 2"

unsigned exp sig
  | exp > 971            = 2047 * 2^52                            -- Inf
--  | exp < (-1023-51-52)  = 0                                      -- 0
  | exp < -1022-52     = round $ fromIntegral sig / fromIntegral (2^(-1023-51-exp))            -- subnormal
  | otherwise            = (exp + 1023+52) * 2^52 + (sig - 2^52)  -- normal



{-
%{{{ saturate
Reconcile Haskell special values with SPU extended precision by saturating results of doubles:
-}
saturate :: Double -> Double
saturate x | x >= maxSPUFloat                       =  maxSPUFloat
           | x <= -maxSPUFloat                      =  -maxSPUFloat
           | x < minSPUFloat && x > -minSPUFloat  = 0
           | Prelude.isNaN x                                = error "can't saturate NaN"
           | otherwise
              = signum x * encodeFloat mant exp
              where (mant',exp) = decodeFloat (signum x * x)
                    mant = mant' .&. (2^(digits+1) - 2^(digits-23))

{-
%}}}

Maximum absolute value SPU float:
-}
minSPUFloat, maxSPUFloat :: Double
maxSPUFloat = 2^(128+1)-2^(128-23)
minSPUFloat = scaleFloat (-126) 1
