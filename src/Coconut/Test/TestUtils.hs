{-
\section{Helper functions for testing SPU functions}
LMD: I only put the minimal required functions that are used in the MASS
code, to avoid as many external dependencies as possible
-}
{-# LANGUAGE NoMonomorphismRestriction#-}
{-# LANGUAGE TypeApplications #-}
module Coconut.Test.TestUtils where

import Coconut.BaseTypes
import Coconut.Core.CoreISA
import Coconut.Core.Interp
import Numeric
-- import PrelExts
import Text.Printf
import Data.List
import Data.Function (on)
import System.Random
import System.Time
import Control.Monad
import Control.Parallel
import Control.Parallel.Strategies
import Coconut.Utils.ArbFloat (dbl2Word64)

{-
%}}}

%{{{ padTo, padTruncateTo, padLeftTo
The following padding functions will \emph{not} truncate |xs|
if it is longer than |k|,
but it will pad it to length |k| if it is shorter:

-}
padTo k x xs = xs ++ replicate (k - length xs) x

{-

Padding on the left instead:
-}
padLeftTo k x xs = replicate (k - length xs) x ++ xs

{-

Formatting debug print for functions returning (result,debug).
-}

-- DOUBLE Precision
debugOut,dbxOut :: (String, Interp VR) -> String
debugOut (name,v1) = (take 40 $ name ++ "                                         ")
  ++ " | " ++ (concatMap ((take 25) . (++"                         ") . show) $ doubles v1)
  ++ (concat $ zipWith (++) ["| ",", ",", ",", ","| ",", "] $ map (reverse . (take 16) . reverse . ("                     " ++ ) . (flip showHex "")) $ dwrds v1)

dbxOut (name,v1) = (take 30 $ name ++ "                                         ")
  ++ " | " ++ (concatMap ((take 25) . (++"                         ") . show) $ doubles v1)
  ++ (concat $ zipWith (++) ["| (0x",",0x",",0x",",0x"] $ map (reverse . (take 8) . reverse . ("0000000000" ++ ) . (flip showHex "")) $ wrds v1)
  ++ ")"

debugOut1 :: (String, Interp VR) -> String
debugOut1 (name,v1) = (take 18 $ name ++ "                        ")
  ++ " | " ++ (concatMap ((take 25) . (++"                         ") . show) $ take 1 $ doubles v1)
  ++ (concat $ zipWith (++) ["| ","| "] $ map (reverse . (take 16) . reverse . ("                     " ++ ) . (flip showHex "")) $ take 1 $ dwrds v1)

-- SINGLE Precision
debugOutS,dbxOutS :: (String, Interp VR) -> String
debugOutS (name,v1) = (take 40 $ name ++ "                                         ")
  ++ " | " ++ (concatMap ((take 12) . (++"                         ") . show) $ floats v1)
  ++ (concat $ zipWith (++) ["| ",", ",", ",", ","| ",", "] $ map (reverse . (take 8) . reverse . ("                     " ++ ) . (flip showHex "")) $ wrds v1)

dbxOutS (name,v1) = (take 30 $ name ++ "                                         ")
  ++ " | " ++ (concatMap ((take 25) . (++"                         ") . show) $ floats v1)
  ++ (concat $ zipWith (++) ["| (0x",",0x",",0x",",0x"] $ map (reverse . (take 8) . reverse . ("0000000000" ++ ) . (flip showHex "")) $ wrds v1)
  ++ ")"

debugOut1S :: (String, Interp VR) -> String
debugOut1S (name,v1) = (take 18 $ name ++ "                        ")
  ++ " | " ++ (concatMap ((take 12) . (++"                         ") . show) $ take 1 $ floats v1)
  ++ (concat $ zipWith (++) ["| ","| "] $ map (reverse . (take 8) . reverse . ("                     " ++ ) . (flip showHex "")) $ take 1 $ wrds v1)

debugQ = True

allOrOneM = concatMap allOrOne
allOrOne (name,var@(v0,v1,v2,v3)) = if debugQ then zipWith (\ i v -> (name++show i,v)) [0..] [v0,v1,v2,v3]
                                              else [(name,v0)]

headX f l (x:_) = x
headX f l _ = error ("head [] at " ++ f ++ ":" ++ show l)

dmat :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> Double -> [Double] -> IO ()
dmat f g ea er x = do putStrLn header
                      putStrLn (replicate 110 '-')
                      mapM putStrLn rows
                      putStrLn []
                      putStrLn ("Values failed: " ++ show (length rows))
                      putStrLn ("Values tested: " ++ show (length x))
                      putStrLn []
                      putStrLn ("Percent goodness: " ++ show (100 * (1 - fromIntegral (length rows) / fromIntegral (length x))) ++ "%")
  where header = fillRight 25 "Input Argument"
              ++ fillRight 25 "Absolute Error"
              ++ fillRight 25 "Relative Error"
              ++ fillRight 25 "Haskell"
              ++ fillRight 25 "SPU"
        rows  = map showRow $ filter isBad
              $ map (\ y -> (y, (dabserr f g y) * 2^53, (drelerr f g y) * 2^53,g y,dtest f y))
              $ map ((headX "testUtils" 72) . (doubles @Interp) . undoubles2) x
        isBad (_,a,r,_,_) = a >= ea || abs(r) >= er
        showRow (y,a,r,h,s) = fillRight 22 (show y)
                       ++ fillRight 25 (toShow a ea)
                       ++ fillRight 25 (toShow r er)
                       ++ fillRight 25 (show h)
                       ++ fillRight 25 (show s)
        toShow z ez | abs(z) >= ez = show z
                    | otherwise = []

dmat2 :: (Interp VR -> Interp VR) -> (Double -> Double) -> Double -> Double -> [Double] -> IO ()
dmat2 f g aemin remin xs = do
    putStrLn $ printf "%9s | %9s | %9s | %9s | %9s | %16s | %16s | %16s"
        "abserr" "relerr" "xtest" "ytest" "ytrue" "xtest" "ytest" "ytrue"
    let results = filter isBad $ parMap rdeepseq (singleTest f g fp64) xs
    mapM_ (putStrLn . showRow) results
    putStrLn ""
    let percBad = 100*(fromIntegral . length $ results)/(fromIntegral . length $ xs)
    putStrLn $ printf "%d bad values out of %d tested (%.1f%% good, %.1f%% bad)"
        ((length results)::Int) ((length xs)::Int) ((100-percBad)::Double) ((percBad)::Double)
    where
        showRow (v,ytest,ytrue,ae,re) = printf "%9.3e | %9.3e | %9.3e | %9.3e | %9.3e | %016X | %016X | %016X"
            (ae::Double) (re::Double) (v::Double) (ytest::Double) (ytrue::Double)
            ((fromIntegral $ dbl2Word64 v)::Integer) ((fromIntegral $ dbl2Word64 ytest)::Integer) ((fromIntegral $ dbl2Word64 ytrue)::Integer)
        isBad (_,_,_,ae,re) = ae >= aemin || (abs re) >= remin

fillRight n = padLeftTo n ' '

dabserr f g x = abs (dtest f x - g x)
dabsrelerr f g x = abs (dabserr f g x / g x)
drelerr f g x = (dtest f x - g x) / abs (g x)
dabs f g e x = filter ((> e) . snd) (zip x (map (dabserr f g) x))
derr f g e x = filter ((> e) . snd) (zip x (map (dabsrelerr f g) x))

dtest :: (Interp VR -> Interp VR) -> Double -> Double
dtest f x =  if (undoubles . (\ [a,b] -> [b,a]) . doubles $ (f xs)) == (f xs)
             then fx1
             else error $ "input "++show x++" gave output "++show fxs
                          ++" ("++showHex (integer @Interp xs) ")"
  where
    fxs@[fx1,fx2] = doubles $ f xs
    xs = undoubles2 x

-- Randomly test a function and output a report every N tests.

data FPFormat = FP { sigBits :: Integer, expBits :: Integer }

fp32 = FP 23 8
fp64 = FP 52 11

randTest64 f g n (low,high) = randTest f g n fp64 (low,high)
randTest32 f g n (low,high) = randTest f g n fp32 (low,high)

singleTest :: (Interp VR -> Interp VR) -> (Double -> Double) -> FPFormat -> Double -> (Double, Double, Double, Double, Double)
singleTest f g fp v = (v, dtest f v, g v, (dabserr f g v) / (ulp fp $ g v), (drelerr f g v) / (eps fp))

randTest :: (Interp VR -> Interp VR) -> (Double -> Double) -> Int -> FPFormat -> (Double, Double) -> IO ()
randTest f g n fp (low,high) =
    (putStrLn $ printf "%10s | %14s | %9s | %9s | %9s | %16s | %16s | %16s"
        "#" "error" "xtest" "ytest" "ytrue" "xtest" "ytest" "ytrue") >>
    runTests 1 (0,0) (0,0)
    where
        runTests i (gmav,gmae) (gmrv,gmre)  = do
            results <- replicateM n doTest
            let (mav,mae,_) = maximumBy (compare `on` getAbs) results
            let (mrv,_,mre) = maximumBy (compare `on` getRel) results
            let ytest = dtest f gmav
            let ytrue = g gmav
            putStrLn $ printf "%10d | %9.3e ulps | %9.3e | %9.3e | %9.3e | %016X | %016X | %016X"
                ((i*n)::Int) (gmae::Double) (gmav::Double) (ytest::Double) (ytrue::Double)
                ((fromIntegral $ dbl2Word64 gmav)::Integer) ((fromIntegral $ dbl2Word64 ytest)::Integer) ((fromIntegral $ dbl2Word64 ytrue)::Integer)
            runTests (i+1) (maximumBy (compare `on` snd) [(mav,mae),(gmav,gmae)]) (maximumBy (compare `on` snd) [(mrv,mre),(gmrv,gmre)])
        doTest = do
            v <- randomRIO (low,high)
            let (_,_,_,ae,re) = singleTest f g fp v
            return (v,ae,re)
        getAbs (_,x,_) = x
        getRel (_,_,x) = x

ulp64 = ulp fp32
ulp32 = ulp fp64

eps32 = eps fp32
eps64 = eps fp64

eps :: FPFormat -> Double
eps (FP sigb _) = 2**(fromIntegral $ -sigb-1)

ulp :: FPFormat -> Double -> Double
ulp (FP sigb expb) x =
    if abs x < minNormal then (ulp (FP sigb expb) minNormal) else 2**(exponent-(fromIntegral $ sigb))
    where
        bias = 2**(fromIntegral $ expb-1)-1
        minNormal = 2**(1-bias)
        exponent = fromIntegral $ floor $ Prelude.log (abs x) / Prelude.log 2
lst2Q xs = let [x0,x1,x2,x3,x4,x5,x6,x7] = take 8 $ padTo 8 0 xs
           in ([x0,x1],[x2,x3],[x4,x5],[x6,x7])

worstError = (\ x->((/(Prelude.log 2)) $ Prelude.log x,x)) . maximum . map maximum . map (map abs)

timeStamp :: a -> IO (a,ClockTime)
timeStamp a = liftM (\ x -> (a,x)) getClockTime
