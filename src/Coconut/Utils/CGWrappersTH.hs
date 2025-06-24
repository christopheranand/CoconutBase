{-# LANGUAGE TemplateHaskell #-}

module Coconut.Utils.CGWrappersTH where

import Language.Haskell.TH

import Control.Monad (unless, mapM)
import Control.DeepSeq (force)

generateNUnwrappableTuples :: Int -> Q [Dec]
generateNUnwrappableTuples n = fmap concat $ mapM generateUnwrappableTuples [2..n]

generateUnwrappableTuples :: Int -> Q [Dec]
generateUnwrappableTuples size = do
  unless (size > 0) $ do
    fail $ "Non-positive size: " ++ size'
  pure [iDecl]
  where
    -- Size string, class name, var names
    size' = show size
    className = mkName "Unwrappable"
    vars = [mkName ('a' : show n) | n <- [1..size]]
    [hardwareVar, xsVar, wrapFuncName, unwrapFuncName,
     unwrappedVar, idStrVar, nodetypeVar] = map mkName ["h", "xs", "wrap", "unwrap", "Unwrapped", "idStr", "nodeType"]
    forceName = mkName "force"
    nfDataName = mkName "NFData"

    -- Common data structures used in the instance declaration
    -- (a1,...,an)
    tupleN = foldl (\acc var -> AppT acc (VarT var)) (TupleT size) vars
    -- (Unwrapped a1,...,Unwrapped an)
    tupleUnwrappedN = foldl (\acc var -> AppT acc (AppT (ConT unwrappedVar) (VarT var))) (TupleT size) vars
    -- [a1,...,an]
    listPatN = ListP $ map (BangP . VarP) vars
    -- (force $ wrap [a1],...,force $ wrap [an])
    wrapSingletons = TupE $ map (\p -> Just $ AppE (VarE forceName) (AppE (VarE wrapFuncName) (ListE [VarE p]))) vars

    -- Unwrappable h _ (partially applied)
    instanceCtx = AppT (AppT (ConT className) (VarT hardwareVar))
    -- Unwrappable h (a1,...,an)
    instanceSig = instanceCtx tupleN

    nfDataCtx = AppT (ConT nfDataName)

    -- instance (Unwrappable h a1, ..., Unwrappable h an) => Unwrappable h (a1,...,an) where
    iDecl = InstanceD Nothing ((map (instanceCtx . VarT) vars)
                              ++ (map (nfDataCtx . VarT) vars)) instanceSig [tySynUnwrapped, funWrap, funUnwrap, funIdStr, funNodeType]
    --   type Unwrapped (a1,...,an) = (Unwrapped a1,...,Unwrapped an)
    tySynUnwrapped = TySynInstD (TySynEqn Nothing (AppT (ConT unwrappedVar) tupleN) tupleUnwrappedN)
    --   wrap !xs = case xs of [!a1,...!an] -> (force $ wrap [a1],...,force $ wrap [an]); _ -> error "Unwrappable.wrap list length mismatch"
    funWrap = FunD wrapFuncName
                 [Clause [BangP $ VarP xsVar]
                  (NormalB (CaseE (VarE xsVar)
                             [ Match listPatN (NormalB wrapSingletons) []
                             , Match WildP (NormalB (InfixE (Just (VarE (mkName "error"))) (VarE $ mkName "$")
                                                     (Just (InfixE (Just (LitE (StringL $ "Unwrappable.wrap list length mismatch " ++ show wrapFuncName++ ", expected length " ++ size' ++ " got length: ")))
                                                            (VarE $ mkName "++") (Just (AppE (VarE $ mkName "show") (AppE (VarE $ mkName "length") (VarE xsVar)))))))) []
                             ]
                           )) []]
    --   unwrap (a1,...,an) =  concat [unwrap a1,...,unwrap an]
    funUnwrap = FunD unwrapFuncName [Clause [TupP $ map (BangP . VarP) vars]
                                     (NormalB $ -- AppE (VarE forceName) $
                                      AppE (VarE (mkName "concat")) (ListE $ map (AppE (VarE $ mkName "unwrap") . VarE) vars)) []]
    --   idStr = idStr @h @a1 ++ ... ++ idStr @h @an ++ []
    funIdStr = ValD (VarP idStrVar)
      (NormalB (foldr (\n acc -> AppE (AppE (VarE (mkName "++"))
                                           ((AppTypeE (AppTypeE (VarE idStrVar) (VarT hardwareVar)) (VarT n))))
                                     acc
                      ) (ConE (mkName "[]")) vars))
      []
    --    nodeType = nodeType @h @a1 ++ nodeType @h @an ++ []
    funNodeType = ValD (VarP nodetypeVar)
      (NormalB (foldr (\n acc -> AppE (AppE (VarE (mkName "++"))
                                           ((AppTypeE (AppTypeE (VarE nodetypeVar) (VarT hardwareVar)) (VarT n))))
                                     acc
                      ) (ConE (mkName "[]")) vars))
      []

generateNTupleData :: Int -> Q [Dec]
generateNTupleData n = fmap concat $ mapM genTupleNFData [10..n]

-- The default instances of NFData only provide up to 9-tuples
-- This allows us to generate more
genTupleNFData :: Int -> Q [Dec]
genTupleNFData size = do
  pure [nfdataIDecl,nfData1IDecl,nfData2IDecl]
  where
    -- Variable declarations
    [nfdataVar, rnfVar, rnf2Var, seqVar] = map mkName ["NFData", "rnf", "rnf2", "seq"]
    [x2ndLast, xLast] = map (mkName . ("x" ++) . show) [size-1, size]
    xVarsExcludeLast2 = map (mkName . ("x" ++) . show) [1.. size-2]
    xVars = map (mkName . ("x" ++) . show) [1..size]
    [rVar, rVar'] = map mkName ["r", "r'"]
    [liftRnfVar, liftRnf2Var] = map mkName ["liftRnf", "liftRnf2"]
    vars = [mkName ('a' : show n) | n <- [1..size]]

    -- Common data structures used in instance declaration
    -- (a1,...,an)
    tupleN varList = foldl (\acc var -> AppT acc (VarT var)) (TupleT size) varList
    -- (NFData a1, ..., NFData an)
    tupleNFDataN = foldl (\acc var -> AppT acc (AppT (ConT nfdataVar) (VarT var))) (TupleT size) vars

    -- NFData($n) (a1,...,an)
    nfDataAppTupleN nfDataName varList =
      AppT (ConT $ mkName nfDataName) $ tupleN varList
    -- instance implementations
    -- rnf = rnf2 (used in NFData)
    rnfEqn = ValD (VarP rnfVar) (NormalB $ VarE rnf2Var) []
    -- liftRnf = liftRnf2 (used in NFData1)
    liftRnfEqn = ValD (VarP liftRnfVar) (NormalB $ AppE (VarE liftRnf2Var) (VarE rnfVar)) []
    seqChain = foldr (\var acc ->
                        InfixE
                        (Just $ AppE (VarE rnfVar) (VarE var))
                        (VarE seqVar)
                        (Just acc)
                     ) (InfixE
                        (Just $ AppE (VarE rVar) (VarE x2ndLast))
                        (VarE seqVar)
                        (Just $ AppE (VarE rVar') (VarE xLast))
                       ) xVarsExcludeLast2
    liftRnf2Eqn = FunD liftRnf2Var
      [Clause
        [VarP rVar, VarP rVar', TupP $ map VarP xVars]
        (NormalB seqChain) []
      ]


    nfdataIDecl = InstanceD Nothing
      (map (AppT (ConT nfdataVar) . VarT) vars)
      (nfDataAppTupleN "NFData" vars) [rnfEqn]
    nfData1IDecl = InstanceD Nothing
      (map (AppT (ConT nfdataVar) . VarT) $ init vars)
      (nfDataAppTupleN "NFData1" $ init vars) [liftRnfEqn]
    nfData2IDecl = InstanceD Nothing
      (map (AppT (ConT nfdataVar) . VarT) $ init (init vars))
      (nfDataAppTupleN "NFData2" $ init (init vars)) [liftRnf2Eqn]
