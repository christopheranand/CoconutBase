-- |
-- Module      :  Coconut.Core.CodeGraph
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a functionality generating a @CodeCoreGraph@ (i.e. via CoconutHypergraph)
-- from the @CoreISA@ DSL

{-# LANGUAGE FlexibleInstances,ScopedTypeVariables,InstanceSigs,TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Coconut.Core.CodeGraph where

import Data.Word
import Data.Int
import Data.Bits ((.&.))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BS
import GHC.TypeLits (KnownNat)

import Coconut.BaseTypes
import Coconut.Core.CoreISA
import qualified Coconut.Core.Interp as Interp
import Coconut.Core.Printer
import Coconut.Core.CoreHardware
import Coconut.Utils.CGWrappers

import Coconut.Graph.CodeGraph
import Coconut.Graph.DataFlow
import Coconut.Utils.ArbFloat (dbl2Word64, dbl2Word32)
import GHC.Float (float2Double)

import Debug.Trace

-- TODO (Lucas): Temporary Z-instructions import
import Coconut.Utils.ArbFloat

tupleWord64ToListInt :: (Word64, Word64) -> [Int]
tupleWord64ToListInt (h, l) = map fromIntegral [h, l]

-- testCG :: CodeGraph NodeName (ResType CORE) (EL CORE)
-- testCG :: CodeGraph CORE
-- testCG = cgFrom (test1 @(Graph CORE))

-- FIXME removed Eq (RegType h),Show (RegType h) constraints, is this cool now?
instance Hardware h => CoreISA (Graph h) where
-- {-
-- Getting values into GPRs
-- -}
    unintegerG :: forall h. (Hardware h) => Word64 -> Graph h GPR
    unintegerG i   = {-# SCC "Core.CodeGraph.unintegerG"   #-}
                    op_c0 @h (Instruction [fromEnum i] "unintegerG" ((unintegerG @Interp) i))
                             (runInterpGPR ((unintegerG @Interp) i)
                             ,runInterpGPR ((unintegerG @Interp) i))
    unwrd64 :: forall h. (Hardware h) => Word64 -> Graph h GPR
    unwrd64 i      = {-# SCC "Core.CodeGraph.unwrd64"      #-}
                    op_c0 @h (Instruction [fromEnum i] "unwrd64" ((unwrd64 @Interp) i))
                             (runInterpGPR ((unwrd64 @Interp) i)
                             ,runInterpGPR ((unwrd64 @Interp) i))
    unwrdsG :: forall h. (Hardware h) => [Word32] -> Graph h GPR
    unwrdsG i      = {-# SCC "Core.CodeGraph.unwrdsG"      #-}
                    op_c0 @h (Instruction (map fromIntegral i) "unwrdsG" ((unwrdsG @Interp) i))
                             (runInterpGPR ((unwrdsG @Interp) i)
                             ,runInterpGPR ((unwrdsG @Interp) i))
{-
Getting values into CRs
-}
    -- TODO fix unwrd64CR, unintegerCR
    unwrd64CR :: forall h. (Hardware h) => Word64 -> Graph h CR
    unwrd64CR w = op_c0 @h (Instruction [fromIntegral w] "unwrd64CR" ((unwrd64CR @Interp) w))
                             (runInterpCR ((unwrd64CR @Interp) w)
                             ,runInterpCR ((unwrd64CR @Interp) w))

    unintegerCR :: forall h. (Hardware h) => Integer -> Graph h CR
    unintegerCR i = op_c0 @h (Instruction [fromIntegral i] "unintegerCR" ((unintegerCR @Interp) i))
                             (runInterpCR ((unintegerCR @Interp) i)
                             ,runInterpCR ((unintegerCR @Interp) i))

    gprToCR :: forall h. (Hardware h) => Graph h GPR -> Graph h CR
    gprToCR (Graph cg s m) = Graph @h @CR cg s m-- simply swaps the phantom type, nothing more

    cmpGPRs :: forall h. (Hardware h) => Graph h GPR -> Graph h GPR -> Graph h CR
    cmpGPRs = op2 @h (Instruction [] "cmpGPRs" (cmpGPRs @Interp))

    cmpVRs :: forall h. (Hardware h) => Graph h VR -> Graph h VR -> Graph h CR
    cmpVRs = op2 @h (Instruction [] "cmpVRs" (cmpVRs @Interp))

-- {-
-- Getting values into VRs
-- -}
    -- replace me with special case of vldxMR?
    uninteger :: forall h. (Hardware h,Eq (RegType h),Show (RegType h)) => Integer -> Graph h VR
    uninteger i    = {-# SCC "Core.CodeGraph.uninteger"    #-}
                    op_c0 @h (Instruction (tupleWord64ToListInt $ runInterpVR $ (uninteger @Interp) i) "uninteger" ((uninteger @Interp) i))
                             (runInterpVR ((uninteger @Interp) i))
    unintegerS :: forall h. (Hardware h,Eq (RegType h),Show (RegType h)) => Integer -> Graph h VR
    unintegerS i   = {-# SCC "Core.CodeGraph.unintegerS"   #-}
                    op_c0 @h (Instruction (tupleWord64ToListInt $ runInterpVR $ (unintegerS @Interp) i) "unintegerS" ((unintegerS @Interp) i))
                             (runInterpVR ((unintegerS @Interp) i))
    undwrds :: forall h. (Hardware h,Eq (RegType h),Show (RegType h)) => [Word64] -> Graph h VR
    undwrds imm    = {-# SCC "Core.CodeGraph.undwrds"      #-}
                    op_c0 @h (Instruction (tupleWord64ToListInt $ runInterpVR $ (undwrds @Interp) imm) "undwrds" ((undwrds @Interp) imm))
                             (runInterpVR ((undwrds @Interp) imm))
    undoubles :: forall h. (Hardware h,Eq (RegType h),Show (RegType h)) => [Double] -> Graph h VR
    undoubles imm  = {-# SCC "Core.CodeGraph.undoubles"    #-}
                    op_c0 @h (Instruction (tupleWord64ToListInt $ runInterpVR $ (undoubles @Interp) imm) "undoubles" ((undoubles @Interp) imm))
                             (runInterpVR ((undoubles @Interp) imm))
    unfloats :: forall h. (Hardware h,Eq (RegType h),Show (RegType h)) => [Double] -> Graph h VR
    unfloats imm   = {-# SCC "Core.CodeGraph.unfloats"     #-}
                    op_c0 @h (Instruction (tupleWord64ToListInt $ runInterpVR $ (unfloats @Interp) imm) "unfloats" ((unfloats @Interp) imm))
                             (runInterpVR ((unfloats @Interp) imm))
    unnibbles :: forall h. (Hardware h,Eq (RegType h),Show (RegType h)) => [Word8] -> Graph h VR
    unnibbles imm  = {-# SCC "Core.CodeGraph.unnibbles"    #-}
                    op_c0 @h (Instruction (tupleWord64ToListInt $ runInterpVR $ (unnibbles @Interp) imm) "unnibbles" ((unnibbles @Interp) imm))
                             (runInterpVR ((unnibbles @Interp) imm))
    unbytes :: forall h. (Hardware h,Eq (RegType h),Show (RegType h)) => [Word8] -> Graph h VR
    unbytes imm    = {-# SCC "Core.CodeGraph.unbytes"      #-}
                    op_c0 @h (Instruction (tupleWord64ToListInt $ runInterpVR $ (unbytes @Interp) imm) "unbytes" ((unbytes @Interp) imm))
                             (runInterpVR ((unbytes @Interp) imm))
    unbits :: forall h. (Hardware h,Eq (RegType h),Show (RegType h)) => [Word8] -> Graph h VR
    unbits imm     = {-# SCC "Core.CodeGraph.unbits"       #-}
                    op_c0 @h (Instruction (tupleWord64ToListInt $ runInterpVR $ (unbits @Interp) imm) "unbits" ((unbits @Interp) imm))
                             (runInterpVR ((unbits @Interp) imm))
    unshorts :: forall h. (Hardware h,Eq (RegType h),Show (RegType h)) => [Word16] -> Graph h VR
    unshorts imm   = {-# SCC "Core.CodeGraph.unshorts"     #-}
                    op_c0 @h (Instruction  (tupleWord64ToListInt $ runInterpVR $ (unshorts @Interp) imm) "unshorts" ((unshorts @Interp) imm))
                             (runInterpVR ((unshorts @Interp) imm))
    unwrds :: forall h. (Hardware h,Eq (RegType h),Show (RegType h)) => [Word32] -> Graph h VR
    unwrds imm     = {-# SCC "Core.CodeGraph.unwrds"       #-}
                    op_c0 @h (Instruction (tupleWord64ToListInt $ runInterpVR $ (unwrds @Interp) imm) "unwrds" ((unwrds @Interp) imm))
                             (runInterpVR ((unwrds @Interp) imm))

    unint32s :: forall h. (Hardware h,Eq (RegType h),Show (RegType h)) => [Int32] -> Graph h VR
    unint32s imm     = {-# SCC "Core.CodeGraph.unint32s"       #-}
                    op_c0 @h (Instruction (tupleWord64ToListInt $ runInterpVR $ (unint32s @Interp) imm) "unint32s" ((unint32s @Interp) imm))
                             (runInterpVR ((unint32s @Interp) imm))
-- -- {-
-- --  Retrieving values from GPRs
-- -- -}
    unsignedG   = error "Core.CodeGraph: trying to get unsignedG value from graph node"
    signedG     = error "Core.CodeGraph: trying to get signedG value from graph node"
    wrd64       = error "Core.CodeGraph: trying to get wrd64 value from graph node"
    wrdsG       = error "Core.CodeGraph: trying to get wrdsG value from graph node"
-- -- {-
-- --  Retrieving values from VRs
-- -- -}
    integer   = error "Core.CodeGraph: trying to get integer value from graph node"
    integerS  = error "Core.CodeGraph: trying to get integerS value from graph node"
    hexval    = error "Core.CodeGraph: trying to get hexval from graph node"
    dwrds     = error "Core.CodeGraph: trying to get dwrds value from graph node"
    int64s    = error "Core.CodeGraph: trying to get int64s value from graph node"
    doubles   = error "Core.CodeGraph: trying to get doubles value from graph node"
    floats    = error "Core.CodeGraph: trying to get floats value from graph node"
    nibbles   = error "Core.CodeGraph: trying to get nibbles value from graph node"
    bytes     = error "Core.CodeGraph: trying to get bytes value from graph node"
    int8s     = error "Core.CodeGraph: trying to get int8s value from graph node"
    bits      = error "Core.CodeGraph: trying to get bits value from graph node"
    shorts    = error "Core.CodeGraph: trying to get shorts value from graph node"
    int16s    = error "Core.CodeGraph: trying to get int16s value from graph node"
    wrds      = error "Core.CodeGraph: trying to get wrds value from graph node"
    int32s    = error "Core.CodeGraph: trying to get int32s value from graph node"

  {- Spilling -}
    spillVR :: Hardware h => Graph h VR -> Graph h SPILL
    spillVR v0 = op1 @h (Spill "spillVR") v0

    despillVR :: Hardware h => Graph h SPILL -> Graph h VR
    despillVR s0 = op1 @h (Despill "despillVR") s0

    spillGPR :: Hardware h => Graph h GPR -> Graph h SPILL
    spillGPR g0 = op1 @h (Spill "spillGPR") g0

    despillGPR :: Hardware h => Graph h SPILL -> Graph h GPR
    despillGPR s0 = op1 @h (Spill "despillGPR") s0

    -- scratchStoreVR  :: Graph h MR -> Int -> Graph h VR  -> Graph h MR
    -- scratchStoreVR m d v  = {-# SCC "Core.CodeGraph.scratchStoreVR"       #-}
    --   op_m2 @h (ScratchStore [fromIntegral d] "scratchStoreVR") (0,0) m v

    -- scratchLoadVR  :: Graph h MR -> Int -> (Graph h VR, Graph h MR)
    -- scratchLoadVR m d = {-# SCC "Core.CodeGraph.scratchLoadVR"       #-}
    --   op_m1 @h (ScratchLoad [fromIntegral d] "scratchLoadVR") (0,1) m

   {- Move Instructions -}

    movVR :: Graph h VR -> Graph h VR
    movVR             = {-# SCC "Core.CodeGraph.vlr"          #-}
                        op1 @h (Instruction [] "movVR" (movVR @Interp))

    movGPR :: Graph h GPR -> Graph h GPR
    movGPR             = {-# SCC "Core.CodeGraph.vlr"          #-}
                        op1 @h (Instruction [] "movGPR" (movGPR @Interp))

-- {- Memory (load/store) instructions
--  -}
    initMR :: forall n h. (Hardware h,Eq (RegType h),Show (RegType h))
           => Integer -> String -> [Interp VR] -> Graph h MR
    initMR size label vs = {-# SCC "Core.CodeGraph.initMR"    #-}
      let
        headX :: Show a => String -> a -> [x] -> x
        headX f l (x:_) = x
        headX f l _ = error ("head [] at " ++ f ++ ":" ++ show l)

        size' = fromIntegral size
      in
      Graph (headX "initMR " label
            -- TODO make sure initMR's have unique labels when inserting with cgbAddMRTable
            <$> (do (n,ns) <- cgbAddInstructionIfNew []
                              (InitMR label $ take size' $ concatMap dwrds vs)
                              [MemoryRes size']
                    cgbAddMRTable label n vs
                    return ns)) ("initMR_"<> (BS.toShort $ BC.pack label)) Nothing

    incMR :: Graph h MR -> Graph h GPR -> Graph h MR
    incMR m g = {-# SCC "Core.CodeGraph.incMR"       #-}
      op2 @h (Instruction [] "incMR" -- NOTE doesn't use op_m1 because this produces a new MR
              (\m0 g0 -> incMR @Interp m0 g0)) m g

    incMR0 :: Graph h MR -> Int -> Graph h MR
    incMR0 m d = {-# SCC "Core.CodeGraph.incMR0"       #-}
      op1 @h (Instruction [fromIntegral d] "incMR0" -- NOTE doesn't use op_m1 because this produces a new MR
              (\m0 -> incMR0 @Interp m0 d)) m

    moduloDWLoad  :: Graph h MR -> Graph h MR -> Int -> (Graph h GPR, Graph h MR)
    moduloDWLoad m0 m d = {-# SCC "Core.CodeGraph.moduloDWLoad"       #-}
      op_m2 @h (Instruction [fromIntegral d] "moduloDWLoad"
                 (\m0' m' -> moduloDWLoad @Interp m0' m' d)) (0,1) m0 m

    moduloDWStore  :: Graph h MR -> Graph h MR -> Int ->Graph h GPR  -> Graph h MR
    moduloDWStore m0 m d v  = {-# SCC "Core.CodeGraph.moduloDWStore"       #-}
      op_m3 @h (Instruction [fromIntegral d] "moduloDWStore"
                (\m0' m' v0 -> moduloDWStore @Interp m0' m' d v0)) (0,0) m0 m v

    moduloVLoad  :: Graph h MR -> Graph h MR -> Int -> (Graph h VR, Graph h MR)
    moduloVLoad m0 m d = {-# SCC "Core.CodeGraph.moduloVLoad"       #-}
      op_m2 @h (Instruction [fromIntegral d] "moduloVLoad"
                 (\m0' m' -> moduloVLoad @Interp m0' m' d)) (0,1) m0 m -- becomes VL when MR becomes GPR

    moduloVStore  :: Graph h MR -> Graph h MR -> Int ->Graph h VR  -> Graph h MR
    moduloVStore m0 m d v  = {-# SCC "Core.CodeGraph.moduloVStore"       #-}
      op_m3 @h (Instruction [fromIntegral d] "moduloVStore"
                (\m0' m' v0 -> moduloVStore @Interp m0' m' d v0)) (0,0) m0 m v

    moduloVLoadG  :: Graph h MR -> Int -> Graph h GPR           -> (Graph h VR, Graph h MR) -- VL
    moduloVLoadG m d x    = {-# SCC "Core.CodeGraph.moduloVLoadG"       #-}
      op_m2 @h (Instruction [fromIntegral d] "moduloVLoadG"
                 (\m0 x0 -> moduloVLoadG @Interp m0 d x0)) (0,1) m x -- becomes VL when MR becomes GPR

    moduloVStoreG  :: Graph h MR -> Int -> Graph h GPR -> Graph h VR  -> Graph h MR
    moduloVStoreG m d x v  = {-# SCC "Core.CodeGraph.moduloVStoreG"       #-}
      op_m3 @h (Instruction [fromIntegral d] "moduloVStoreG"
                (\m0 x0 v0 -> moduloVStoreG @Interp m0 d x0 v0)) (0,0) m x v

  {- Generic logical/arithmetic instructions
   -}
    xorG :: Graph h GPR -> Graph h GPR -> Graph h GPR
    xorG x y     = {-# SCC "Core.CodeGraph.xor"       #-}
                    op2 @h (Instruction [] "xorG" (xorG @Interp)) x y

    orG :: Graph h GPR -> Graph h GPR -> Graph h GPR
    orG x y      = {-# SCC "Core.CodeGraph.or"        #-}
                    op2 @h (Instruction [] "orG" (orG @Interp)) x y

    andG :: Graph h GPR -> Graph h GPR -> Graph h GPR
    andG x y     = {-# SCC "Core.CodeGraph.and"       #-}
              op2 @h (Instruction [] "andG" (andG @Interp)) x y

    addG :: Graph h GPR -> Graph h GPR -> Graph h GPR
    addG x y     = {-# SCC "Core.CodeGraph.add"       #-}
                    op2 @h (Instruction [] "addG" (addG @Interp)) x y
    subG :: Graph h GPR -> Graph h GPR -> Graph h GPR
    subG x y    = {-# SCC "Core.CodeGraph.subf"      #-}
                    op2 @h (Instruction [] "subG" (subG @Interp)) x y

  {- Generic branching instructions
   -}
    jump :: Graph h BRANCH
    jump = op_d0 @h (BranchNode "jump" [] (jump @Interp))

    branchNotLow :: Graph h GPR -> Graph h GPR -> Graph h BRANCH
    branchNotLow = op_d2 @h(BranchNode "branchNotLow" [] (branchNotLow @Interp))

    branchLow :: Graph h GPR -> Graph h GPR -> Graph h BRANCH
    branchLow = op_d2 @h(BranchNode "branchLow" [] (branchLow @Interp))

    -- TODO: BranchNode with immediate arguments
    branchImmNotEq :: Graph h GPR -> Int -> Graph h BRANCH
    branchImmNotEq r0 imm = op_d1 @h (BranchNode "branchImmNotEq" [imm] (\r' -> branchImmNotEq @Interp r' imm)) r0

    branchImmNotHigh :: Graph h GPR -> Int -> Graph h BRANCH
    branchImmNotHigh r0 imm = op_d1 @h (BranchNode "branchImmNotHigh" [imm] (\r' -> branchImmNotHigh @Interp r' imm)) r0
