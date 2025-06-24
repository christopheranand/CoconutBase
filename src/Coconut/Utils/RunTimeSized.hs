-- |
-- Module      :  Coconut.Utils.RunTimeSized
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- Interpretation requires runtime memory handling, this module provides utitlities
-- for such functionality

{-# LANGUAGE FlexibleInstances #-}

module Coconut.Utils.RunTimeSized where

import GHC.TypeLits (Nat,KnownNat)

import qualified Data.ByteString as BS
import qualified Data.List as List
import GHC.Arr (inRange)

import Coconut.Utils.MeetJoin
import Coconut.BaseTypes

instance HasMeet () (Interp MR) where
  meet name _eq mrs@(mr1:_) = reset mr1 $ meet' name $
       if length (List.nub sizes) == 1
         then map mkMR mrs
         else error $ "Interp.HasMeet(Interp MR) mismatching sizes" ++ show sizes
    where
      size mr = case mr of InterpMR (PIMR _ _ _ _ s) -> s

      sizes = map size mrs

      mkMR :: Interp MR -> RunTimeSized
      mkMR mr = case mr of InterpMR x -> rts x

      reset x (RTS _ c _a' _o' _) = case x of
        (InterpMR (PIMR n _c _a _o s)) -> InterpMR $ PIMR n c c [] s
  meet name _eq [] = error $ "ZInterp.MR.meet [] "++name

oneAtATime :: [a] -> [(a,[a])]
oneAtATime lst = map (\ (x,y:ys) -> (y,x++ys))
         $ takeWhile (not . null . snd) $ map (flip splitAt lst) [0..]

meet' :: String -> [RunTimeSized] -> RunTimeSized
meet' msg mrs
  | null mrs = error $ "ZInterp.meet("++msg++") of empty list"
  | commonLast && noIntersect = merged
  | otherwise = error $ "ZInterp.meet("++n++"/"++msg++") conflict " ++ show (
      case (commonLast::Bool,noIntersect::Bool) of
             (False,True) -> "no common last"
             (False,False) -> "no common last"
             (True,False) -> "size:"++show (rsize mr) ++ show (map ropsSince mrs)
                  ++ " num loads / stores " ++ show (length $ concatMap rldStIdxs mrs
                                                     ,length $ concatMap rstIdxs mrs)
             (True,True) -> "can't occur")
  where
      (mr : mrr) = mrs
      commonLast = foldr ((\ state bool -> (ratLast mr == state) && bool) . ratLast) True mrr
      noIntersect = all
                (\ (mr, mrs)
                   -> null
                        $ List.intersectBy
                            rangeOverlaps (rstIdxs mr) (concatMap rldStIdxs mrs)) (oneAtATime mrs)
      rangeOverlaps a@(al, ar) b@(bl, br) = inRange a bl || inRange b al
      (RTS n cur atL ops sz)
        = foldr
            (\ (Store idx v) mr
               -> mr {rcurrent = spliceByteString v (rcurrent mr) idx})
            mr
            $ filter isStore $ concatMap ropsSince mrs
      merged = RTS n cur cur [] sz

isStore :: LS -> Bool
isStore (Store _ _) = True
isStore _ = False

isLoad :: LS -> Bool
isLoad (Load _) = True
isLoad _ = False

lsIdx (Load idx) = idx
lsIdx (Store idx _) = idx

rts :: InterpMemRegion -> RunTimeSized
rts x@(PIMR n c a o s) = RTS n c a o (fromIntegral s)

stIdxs mr = map lsIdx $ filter isStore $ opsSince mr
ldStIdxs mr = map lsIdx $ opsSince mr

rstIdxs mr = map lsIdx $ filter isStore $ ropsSince mr
rldStIdxs mr = map lsIdx $ ropsSince mr

-- | insert one bytestring into the middle of another one
spliceByteString src dst (start,end) = BS.concat [BS.take start dst, BS.take (end-start+1) src,
                                                  BS.drop (end+1) dst]
