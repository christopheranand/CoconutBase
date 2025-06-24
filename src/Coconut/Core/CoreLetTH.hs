{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Coconut.Core.CoreLetTH where

import Language.Haskell.TH -- hiding (VarP)

import Control.Monad (unless, replicateM, forM)

import Data.Graph.Inductive (Node)
import Coconut.Graph.CodeGraph

-- generateCoreLetClass :: Int -> Q [Dec]
-- generateCoreLetClass size = pure [ClassD [AppT coreISA repr] className [plainRepr] [] sigGen]
--   where
--     -- Size string, variable names, miscellaneous class names and variables
--     size' = show size
--     varsIn = [mkName ('a' : show n) | n <- [1..size]]
--     varsOut = [mkName ('b' : show n) | n <- [1..size]]
--     sigName inSize outSize = mkName $ "let" ++ show inSize ++ "_" ++ show outSize
--     className = mkName "CoreLet"
--     coreISA = ConT $ mkName "CoreISA"
--     repr = VarT $ mkName "repr"
--     plainRepr = PlainTV $ mkName "repr"

--     -- produces a tuple of (repr a1, ... repr an) for given size 'n' and variable letter 'a'
--     -- Note that size 1 is a special case as we dont want to use the "Unit" tuple
--     reprTuple tupSize varNames = -- AppT (TupleT tupSize)
--       case tupSize of
--         1 -> AppT repr (VarT $ head varNames)
--         _ -> foldl (\acc var -> AppT acc (AppT repr (VarT var))) (TupleT tupSize)
--                    (take tupSize varNames)

--     -- enumerates all n x n class operations for each input and output sizes
--     sigGen =
--       [ SigD (sigName inSize outSize)
--         (AppT (AppT ArrowT (reprTuple inSize varsIn)) -- repr a
--               -- repr a -> repr b
--               (AppT (AppT ArrowT (AppT (AppT ArrowT (reprTuple inSize varsIn))
--                                                     (reprTuple outSize varsOut)))
--                     (reprTuple outSize varsOut) -- repr b
--         ))
--       | inSize <- [1..size]
--       , outSize <- [1..size]
--       ]

-- generateCoreLetInterp :: Int -> Q [Dec]
-- generateCoreLetInterp size = pure [InstanceD Nothing [] (AppT coreLet interp) sigGen]
--   where
--     -- Size string, variable names, miscellaneous class names and variables
--     sigName inSize outSize = mkName $ "let" ++ show inSize ++ "_" ++ show outSize
--     className = mkName "CoreLet"
--     coreLet = ConT $ mkName "CoreLet"
--     interp = ConT $ mkName "Interp"
--     [f, x] = map mkName ["f", "x"]

--     sigGen =
--       [ FunD (sigName inSize outSize) [Clause [VarP x, VarP f] (NormalB (AppE (VarE f) (VarE x))) []]
--       | inSize <- [1..size]
--       , outSize <- [1..size]
--       ]

-- class SimpleUnwrap h a where
--   unwrapTuple :: a -> [(CGBState h Node,Maybe String)]
--   wrapTuple :: [(CGBState h Node,Maybe String)] -> a
--   wrapperTuple :: CGBState h [Node] -> a

-- instance SimpleUnwrap h (Graph h a) where
--   unwrapTuple (Graph a s) = [(a,s)]
--   wrapTuple xs = case xs of
--     [(a,s)] -> Graph a s
--     _ -> error "Length error"
--   wrapperTuple n = wrapTuple [(fmap (!! 0) n,Nothing)]

-- generateSimpleUnwrapInstances :: Int -> Q [Dec]
-- generateSimpleUnwrapInstances size = do
--   unless (size > 1) $ fail $ "Size less than 1: " ++ size'
--   -- wrapperExprTH <- wrapperExpr size
--   fmap concat $ forM [2..size] $ \sz ->
--     [d|
--        instance SimpleUnwrap $(varT h) $(graphTuple sz) where
--          unwrapTuple = $(unwrapTupleExpr sz)
--          wrapTuple xs = case xs of
--            $(listPat sz) -> $(tupExpr sz)
--            _ -> error $ "SimpleUnwrap.wrapTuple list length mismatch, expected length " ++ show sz ++ " got length: " ++ show (length xs)
--          wrapperTuple n = $(wrapperExpr sz)
--      |]

--   where
--     size' = show size
--     vars = [mkName ('a' : show n) | n <- [1..size]]
--     graph = mkName "Graph"
--     [f, e, h, n] = map mkName ["f", "e", "h", "n"]
--     unwrapTupleName = mkName "unwrapTuple"
--     simpleUnwrapName = mkName "SimpleUnwrap"

--     -- produces a tuple of (repr a1, ... repr an) for given size 'n' and variable letter 'a'
--     graphTuple tupSize = -- AppT (TupleT tupSize)
--       foldl (\acc var -> appT acc (appT (appT (conT graph) (varT h)) (varT var))) (tupleT tupSize)
--                    (take tupSize vars)

--     -- produces a tuple pattern match lambda of \(Graph a1, ..., Graph an) -> body
--     tupLam tupSize body =
--       lamE [tupP $ map (conP graph . (:[]) . varP) (take tupSize vars)] body

--     -- produces the list expresson [a1,...,an]
--     listExpr tupSize = listE $ map varE (take tupSize vars)

--     unwrapTupleExpr tupSize = tupLam tupSize (listExpr tupSize)

--     -- produces the list pattern [a1,...,an]
--     listPat tupSize = listP $ map varP (take tupSize vars)

--     -- produces the tuple expression (Graph a1,...,Graph an)
--     tupExpr tupSize = tupE $ map (appE (conE graph) . varE) (take tupSize vars)

--     -- produces the expression (wrapTuple [fmap (!! 0) n],...,wrapTuple [fmap (!! $n) n])
--     -- This takes a function that splices the tuple elements and recombines
--     -- the elements into a tuple using regular TH constructors
--     wrapperExpr tupSize = do
--       tupElems <- mapM (\sz -> pure [|wrapTuple [fmap (!! sz) $(varE n)]|]) [0..tupSize-1]
--       tupE tupElems

-- generateCoreLetGraph :: Int -> Q [Dec]
-- generateCoreLetGraph size = do
--   sigExprQ <- runQ sigExpr
--   pure [InstanceD Nothing [hardwareConstraint] (AppT coreLet graph) (sigGen sigExprQ)]
--   where
--     -- Size string, variable names, miscellaneous class names and variables
--     sigName inSize outSize = mkName $ "let" ++ show inSize ++ "_" ++ show outSize
--     className = mkName "CoreLet"
--     coreLet = ConT $ mkName "CoreLet"
--     graph = AppT (ConT $ mkName "Graph") (VarT h)
--     hardware = ConT $ mkName "Hardware"
--     hardwareConstraint = AppT hardware (VarT h)
--     [f, e, h] = map mkName ["f", "e", "h"]
--     x = mkName "x"
--     sigExpr =
--       [| wrapperTuple $ do
--             $(varP x) <- sequence $ unwrapTuple @($(varT h)) $(varE e)
--             sequence $ unwrapTuple $ $(varE f) $ wrapTuple @($(varT h)) (map return $(varE x))
--         |]

--     -- Due to staging restrictions on splicing, we need to run sigExpr
--     -- in the Q monad before passing the result as an argument to sigGen
--     sigGen sigExprQ =
--         [ FunD (sigName inSize outSize) [Clause [VarP e, VarP f] (NormalB sigExprQ) []]
--         | inSize <- [1..size]
--         , outSize <- [1..size]
--         ]
