-- |
-- Module      :  Coconut.Utils.Literal
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module contains the Literal dataype that's used primarily for providing convienent
-- representations of immediates

{-# LANGUAGE DeriveDataTypeable #-}

module Coconut.Utils.Literal where
-- TODO document Literal
import Data.Typeable  ( Typeable )
import Data.Data      ( Data )
{-

-}
data Literal
  = LitInteger Integer
  | LitDouble Double
  | LitSymbol String
  deriving (Eq, Ord, Typeable, Data)

instance Show Literal where
  showsPrec p (LitInteger i) = showsPrec p i
  showsPrec p (LitDouble d) = showsPrec p d -- \edcomm{WK}{May need to be changed to limited width}
  showsPrec p (LitSymbol s) = (s ++)
{-

... and some useful accessor functions:

-}
isLitSymbol (LitSymbol _) = True
isLitSymbol _ = False

litAsDouble (LitInteger int) = fromIntegral int
litAsDouble (LitDouble dbl) = dbl
litAsDouble (LitSymbol s) = error $ "litAsDouble (LitSymbol " ++ shows s ")"
{-

%{{{ unLitInteger, unLitDouble
-}
litShowUInt (LitInteger i) = show $ i `mod` 2 ^ 32
litShowUInt (LitDouble d) = error $ "litShowUInt " ++ show d
litShowUInt (LitSymbol s) = s

unLitInteger (LitInteger i) = i
unLitInteger (LitDouble d) = error $ "unLitInteger " ++ show d
unLitInteger (LitSymbol s) = error $ "unLitInteger " ++ s

unLitDouble (LitInteger i) = error $ "unLitDouble " ++ show i
unLitDouble (LitDouble d) = d
unLitDouble (LitSymbol s) = error $ "unLitDouble " ++ s
{-
%}}}
-}
