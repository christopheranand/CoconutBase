{-# LANGUAGE FlexibleInstances,ScopedTypeVariables,InstanceSigs,TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Coconut.Core.CoreLet where

import Coconut.BaseTypes
import Coconut.Core.CoreISA

import Coconut.Core.CoreLetTH
import Coconut.Core.Interp
import Coconut.Core.CodeGraph
import Coconut.Graph.CodeGraph

import Coconut.Utils.CGWrappers

import Data.Graph.Inductive (Node)

import Control.Monad
import qualified Data.Bifunctor


{-
class CoreISA repr => CoreLet repr where
  let_ :: repr a -> (repr a -> repr b) -> repr b
  let2_2 :: (repr a1, repr a2) -> ((repr a1, repr a2) -> (repr b1, repr b2)) -> (repr b1, repr b2)
-}

-- $(generateCoreLetClass 4)
-- $(generateCoreLetInterp 4)
-- $(generateSimpleUnwrapInstances 4)
-- $(generateCoreLetGraph 4)

{-
class SimpleUnwrap h a where
  wrapTuple :: [CGBState h Node] -> a
  unwrapTuple :: a -> [CGBState h Node]
  wrapperTuple :: CGBState h [Node] -> a


instance SimpleUnwrap h (Graph h a) where
  unwrapTuple (Graph a) = [a]
  wrapTuple xs = case xs of
    [a] -> Graph a
    _ -> error "Length error"
  wrapperTuple n = wrapTuple [fmap (!! 0) n]

instance SimpleUnwrap h (Graph h a1, Graph h a2) where
  unwrapTuple = \(Graph a1, Graph a2) -> [a1,a2]
  wrapTuple xs = case xs of
    [a1,a2] -> (Graph a1, Graph a2)
    _ -> error "Length error"
  wrapperTuple n =
    (wrapTuple [fmap (!! 0) n], wrapTuple [fmap (!! 1) n])
-}

{-
instance (Hardware h) => CoreLet (Graph h) where
  {-
  let2_2 :: forall h a1 a2 b1 b2.
             (Hardware h,Eq (RegType h),Show (RegType h))
          => (Graph h a1, Graph h a2)
          -> ((Graph h a1, Graph h a2) -> (Graph h b1, Graph h b2))
          -> (Graph h b1, Graph h b2)
  -}
  let2_2 e f = wrapperTuple $ do
    x <- sequence $ unwrapTuple @h e
    sequence $ unwrapTuple $ f $ wrapTuple @h (map return x)
-}
