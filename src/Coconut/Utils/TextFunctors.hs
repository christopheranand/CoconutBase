-- |
-- Module      :  Coconut.Utils.TextFunctors
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module provides show and read functors useful precedence sensitive data

module Coconut.Utils.TextFunctors
 ( module Coconut.Utils.TextFunctors
 , GHC.Read.Read(..)
 ) where

-- TODO document TextFunctors

import qualified GHC.Show
import qualified GHC.Read

-- the following imports are necessary for the read functions:
import Text.ParserCombinators.ReadPrec
import Data.Array
import qualified Text.Read.Lex as L
{-

%{{{ \subsection{Show}
\subsection{Show}

-}
type ShowSPrec a = Int -> a -> ShowS

showsPrecList :: Show a => ShowSPrec a -> ShowSPrec [a]
showsPrecList showsPrec p = GHC.Show.showList__ shows
{-

-}
showsPrecMaybe :: ShowSPrec a -> ShowSPrec (Maybe a)
showsPrecMaybe _showsPrec _p Nothing s = showString "Nothing" s
showsPrecMaybe showsPrec p (Just x) s
                          = (showParen (p > appPrec) $
         showString "Just " .
     showsPrec appPrec1 x) s
{-

-}
showsPrecEither :: ShowSPrec a -> ShowSPrec b -> ShowSPrec (Either a b)
showsPrecEither showsPrecA showsPrecB p e = (showParen (p > appPrec) $
        case e of
         Left  a -> showString "Left "  . showsPrecA appPrec1 a
         Right b -> showString "Right " . showsPrecB appPrec1 b
       )
{-

-}
showsPrecTup2 showsPrecA showsPrecB _ (x,y) = showChar '(' . showsPrecA noPrec x . showChar ',' . showChar ' ' .
                  showsPrecB noPrec y . showChar ')'
{-

-}
showsPrecTup3 showsPrecA showsPrecB showsPrecC _ (x,y,z) s =
  (showChar '(' . showsPrecA noPrec x . showChar ',' . showChar ' ' .
                  showsPrecB noPrec y . showChar ',' . showChar ' ' .
                  showsPrecC noPrec z . showChar ')'
  ) s
{-

-}
showsPrecTup4 showsPrecA showsPrecB showsPrecC showsPrecD _ (x,y,z,u) s =
  (showChar '(' . showsPrecA noPrec x . showChar ',' . showChar ' ' .
                  showsPrecB noPrec y . showChar ',' . showChar ' ' .
                  showsPrecC noPrec z . showChar ',' . showChar ' ' .
                  showsPrecD noPrec u . showChar ')'
  ) s
{-

-}
showsPrecTup5 showsPrecA showsPrecB showsPrecC showsPrecD showsPrecE _ (x,y,z,u,v) s =
  (showChar '(' . showsPrecA noPrec x . showChar ',' . showChar ' ' .
                  showsPrecB noPrec y . showChar ',' . showChar ' ' .
                  showsPrecC noPrec z . showChar ',' . showChar ' ' .
                  showsPrecD noPrec u . showChar ',' . showChar ' ' .
                  showsPrecE noPrec v . showChar ')'
  ) s
{-

-}
noPrec :: Int
noPrec = 0

appPrec = GHC.Show.appPrec
appPrec1 = GHC.Show.appPrec1
{-
%}}}

%{{{ \subsection{Read}
\subsection{Read}

-}
readPrecList = GHC.Read.list
{-

-}
readPrecMaybe :: ReadPrec a -> ReadPrec (Maybe a)
readPrecMaybe readPrec =
    parens
    (do L.Ident "Nothing" <- lexP
        return Nothing
     +++
     prec appPrec (do
           L.Ident "Just" <- lexP
           x              <- step readPrec
           return (Just x))
    )
{-

-}
readPrecEither :: ReadPrec a -> ReadPrec b -> ReadPrec (Either a b)
readPrecEither readPrecA readPrecB =
    parens
    ( prec appPrec
      ( do L.Ident "Left" <- lexP
           x            <- step readPrecA
           return (Left x)
       +++
        do L.Ident "Right" <- lexP
           y             <- step readPrecB
           return (Right y)
      )
    )
{-

-}
readPrecArray :: Ix i => ReadPrec i -> ReadPrec a -> ReadPrec (Array i a)
readPrecArray readPrecI readPrecA = parens $ prec appPrec $ do
  L.Ident "array" <- lexP
  bounds <- step (readPrecTup2 readPrecI readPrecI)
  vals   <- step (GHC.Read.list (readPrecTup2 readPrecI readPrecA))
  return (array bounds vals)
{-

-}
readPrecTup2 readPrecA readPrecB =
    parens
    ( paren
      ( do x <- readPrecA
           L.Punc "," <- lexP
           y <- readPrecB
           return (x,y)
      )
    )
{-

-}
readPrecTup3 readPrecA readPrecB readPrecC =
    parens
    ( paren
      ( do x <- readPrecA
           L.Punc "," <- lexP
           y <- readPrecB
           L.Punc "," <- lexP
           z <- readPrecC
           return (x,y,z)
      )
    )
{-

-}
readPrecTup4 readPrecA readPrecB readPrecC readPrecD =
    parens
    ( paren
      ( do w <- readPrecA
           L.Punc "," <- lexP
           x <- readPrecB
           L.Punc "," <- lexP
           y <- readPrecC
           L.Punc "," <- lexP
           z <- readPrecD
           return (w,x,y,z)
      )
    )
{-

-}
readPrecTup5 readPrecA readPrecB readPrecC readPrecD readPrecE =
    parens
    ( paren
      ( do v <- readPrecA
           L.Punc "," <- lexP
           w <- readPrecB
           L.Punc "," <- lexP
           x <- readPrecC
           L.Punc "," <- lexP
           y <- readPrecD
           L.Punc "," <- lexP
           z <- readPrecE
           return (v,w,x,y,z)
      )
    )
{-

-}
paren = GHC.Read.paren
parens = GHC.Read.parens
lexP = GHC.Read.lexP
