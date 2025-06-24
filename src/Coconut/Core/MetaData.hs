-- |
-- Module      :  Coconut.Core.MetaData
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a functionality generating a implementing CORE hardware functionality

{-# LANGUAGE OverloadedStrings #-}
module Coconut.Core.MetaData where

import Coconut.BaseTypes (MDMap (..), Interp, runInterpVR)
import Coconut.Core.CoreHardware (CORE,MetaData(..))
import Coconut.Core.Printer (InstructionFormat(..))
import Coconut.Core.CoreISA (unwrds)
import Coconut.Core.Interp

import qualified Data.Map as Map

{-
Z-mnemonics
G - 64-bit operands
    (absence of G means 32-bit operands, but sometimes may have mnemonic F as well)
R - Register-register formats, i.e. just pure register manipulation, no memory load/stores involved
K - Indicates no overwriting of designation register, usually implies an additional register involved in the instruction
L - Logical (TODO: Find out what this does)
I - Involves immediates
-}




-- | Used for simulation/printing on instances @Hardware CORE@
coreMetaData :: MDMap CORE
coreMetaData = MDMap $ Map.fromList
               [("undwrds", MDCore "undwrds" 5 (const ConstDecodeVR))
               ,("unwrds", MDCore "unwrds" 5 (const ConstDecodeVR))
               ,("unbytes", MDCore "unbytes" 5 (const ConstDecodeVR))
               ,("unshorts", MDCore "unshorts" 5 (const ConstDecodeVR))
               ,("undoubles", MDCore "undoubles" 5 (const ConstDecodeVR))
               ,("uninteger", MDCore "uninteger" 5 (const ConstDecodeVR))
               ,("unintegerG",MDCore "unintegerG" 5 (const ConstDecodeGPR))
               ,("cmpGPRs",MDCore "cgr" 3 (const RRE))
               ,("ldMR",MDCore "lg" 5 (const RXa))
               ,("ldxMR",undefined)
               ,("lwzMR",undefined)
               ,("stdMR",MDCore "st" 5 undefined)
               ,("stwMR",undefined)
               ,("stdxMR",undefined) -- TODO: Does this map to "std"?
               ,("stvxMR",MDCore "vst" 5 (const $ VRX Nothing False False))
               ,("stv0MR",MDCore "vst" 5 (const $ VRX Nothing False True))
               ,("incMR",MDCore "agrk" 3 (const RRFa))
               ,("initMR", MDCore "larl" 5 (const RILb))
               ,("lgMR",MDCore "lg" 5 (const RXYa))
               ,("vldxMR",MDCore "vl" 5 (const $ VRX Nothing False False))
               ,("vld0MR",MDCore "vl" 5 (const $ VRX Nothing False True))
               ,("vldInMR",undefined)
               ,("fauxDep",undefined)
               ,("vleb",MDCore "vleb" 5 undefined)
               ,("vleh",MDCore "vleh" 5 undefined)
               ,("vlef",MDCore "vlef" 5 undefined)
               ,("vleg",MDCore "vleg" 5 undefined)
               ,("vlegAll",MDCore "vleg" 5 undefined)
               ,("andG",MDCore "ng" 3 (const RXYa))
               ,("andcG",MDCore "ncgrk" 3 undefined)
               ,("orG",MDCore "og" 3 undefined)
               ,("orcG",MDCore "ocgrk" 3 undefined)
               ,("xorG",MDCore "xgrk" 3 (const RRFa))
               ,("xorcG",undefined)
               ,("nandG",MDCore "nngrk" 3 undefined)
               ,("norG",MDCore "nogrk" 3 undefined)
               ,("eqvG",MDCore "nxgrk" 3 undefined)
               ,("addG",MDCore "agrk" 3 (const RRFa))
               ,("subG",MDCore "sgrk" 3 (const RRFa))
               ,("subfG",MDCore "sgrk" 3 (const RRFa))
               ,("negG",undefined)
               ,("mulhdG",undefined)
               ,("mulldG",MDCore "mgrk" 3 (const RRFa)) -- TODO: Z places multiply high result in R1, and low result in R(1+1)
               ,("mulhduG",undefined)
               ,("divdG",undefined)
               ,("divduG",undefined)
               ,("sldG",MDCore "slda" 3 undefined) -- TODO: Is this correct?
               ,("slwG",undefined) -- TODO: Is this a shift left single (sla)?
               ,("sradG",undefined)
               ,("sradiG",undefined)
               ,("srawG",undefined)
               ,("srawG",undefined)
               ,("srawiG",undefined)
               ,("srdG",undefined)
               ,("srwG",undefined)
               ,("rldiclG",undefined)
               ,("rldicrG",undefined)
               ,("rldimiG",undefined)
               ,("rldicG",undefined)
               ,("rldclG",undefined)
               ,("rldcrG",undefined)
               ,("rlwimiG",undefined)
               ,("dfa",MDCore "vfa" 3 (const $ VRRc (Just 3) (Just 0) Nothing))
               ,("dfs",MDCore "vfs" 3 (const $ VRRc (Just 3) (Just 0) Nothing))
               ,("dfm",MDCore "vfm" 3 (const $ VRRc (Just 3) (Just 0) Nothing))
               ,("dfdiv",MDCore "vfd" 3 undefined)
               ,("dfsqrt",MDCore "vfsq" 3 undefined)
               ,("dfma",MDCore "vfma" 3 (const $ VRRe (Just 0) (Just 3)))
               ,("dfms",MDCore "vfms" 3 (const $ VRRe (Just 0) (Just 3)))
               ,("vfch",MDCore "vfch" 3 (const $ VRRc (Just 3) (Just 0) (Just 0)))
               ,("vfche",MDCore "vfche" 3 undefined)
               ,("vfce",MDCore "vfce" 3 undefined)
               ,("vceq",MDCore "vceq" 3 (\[m4] -> VRRb (Just m4) (Just 0) False))
               -- TODO check correct
               ,("vch",MDCore "vch" 3 (\[m4] -> VRRb (Just m4) (Just 0) False))
               ,("vchl",MDCore "vchl" 3 (\[m4] -> VRRb (Just m4) (Just 0) False))
               ,("vmah",MDCore "vmah" 3 undefined)
               ,("vmalh",MDCore "vmalh" 3 undefined)
               ,("vmal",MDCore "vmal" 3 undefined)
               ,("vmh",MDCore "vmh" 3 (\[m4] -> VRRc (Just m4) Nothing Nothing))
               ,("vmlh",MDCore "vmlh" 3 undefined)
               ,("vml",MDCore "vml" 3 undefined)
               ,("va",MDCore "va" 3 (\[m4] -> VRRc (Just m4) Nothing Nothing))
               ,("vs",MDCore "vs" 3 (\[m4] -> VRRc (Just m4) Nothing Nothing))
               ,("vacc",MDCore "vacc" 3 undefined)
               ,("vac",MDCore "vac" 3 undefined)
               ,("vaccc",MDCore "vaccc" 3 undefined)
               ,("vsbi",MDCore "vsbi" 3 undefined)
               ,("vscbi",MDCore "vscbi" 3 (\[m4] -> VRRc (Just m4) Nothing Nothing))
               ,("vsbcbi",MDCore "vsbcbi" 3 undefined)
               ,("vdwrdIdx",MDCore "vpdi" 3 undefined)
               -- ,("selb",MDCore "vsel" 3 (const $ VRRe Nothing Nothing))
               ,("selb",MDCore "vsel" 3 (const $ VRReSWAP Nothing Nothing))
               -- TODO check fi shufb VRRe Nohting Nothing is correct
               ,("shufb",MDCore "vperm" 3 (const $ VRRe Nothing Nothing))
               ,("vbperm",MDCore "vbperm" 3 undefined)
               ,("vmrh",MDCore "vmrh" 3 undefined)
               ,("vmrl",MDCore "vmrl" 3 undefined)
               ,("vslb",MDCore "vslb" 3 undefined)
               -- TODO check if vsldb VRI-d is correct
               ,("vsldb",MDCore "vsldb" 3 (const $ VRId Nothing))
               -- vsl and vsrl work in interp but not on hardware because of swapped argument order
               ,("vsl",MDCore "vsl" 3 undefined) -- FIXME wrong argument order?
               ,("vsrl",MDCore "vsrl" 3 (const $ VRRc Nothing Nothing Nothing)) -- FIXME wrong argument order?
               ,("vsrlb",MDCore "vsrlb" 3 undefined)
               ,("verll",MDCore "verll" 3 $ \[m4, _] -> VRSa (Just m4) False)
               ,("verll0",MDCore "verll0" 3 $ \[m4, _] -> VRSa (Just m4) True)
               ,("vesrl",MDCore "vesrl" 3 $ undefined)
               ,("vesrl0",MDCore "vesrl0" 3 undefined)
               ,("vesra",MDCore "vesra" 3 undefined)
               ,("vesra0",MDCore "vesra" 3 $ \[m4,_] -> VRSa (Just m4) True)
               ,("vesl",MDCore "vesl" 3 undefined)
               ,("vesl0",MDCore "vesl0" 3 undefined)
               ,("verim",MDCore "verim" 3 (const $ VRId (Just 3)))
               ,("verimD",MDCore "verim" 3 undefined)
               ,("vlc",MDCore "vlc" 5 undefined)
               ,("vlp",MDCore "vlp" 5 undefined)
               -- ,("vnot",undefined) -- TODO: Specify this outside of CoreISA, by using vnor
               ,("vand",MDCore "vn" 3 (const $ VRRc Nothing Nothing Nothing))
               ,("vandc",MDCore "vnc" 3 (const $ VRRc Nothing Nothing Nothing))
               ,("vor",MDCore "vo" 3 (const $ VRRc Nothing Nothing Nothing))
               ,("vnor",MDCore "vno" 3 undefined)
               ,("vxor",MDCore "vx" 3 (const $ VRRc Nothing Nothing Nothing))
               ,("vleib",MDCore "vleib" 5 undefined)
               ,("vleih",MDCore "vleih" 5 undefined)
               ,("vleif",MDCore "vleif" 5 undefined)
               ,("vleig",MDCore "vleig" 5 undefined)
               ,("vsum",MDCore "vsum" 3 undefined)
               ,("vsumq",MDCore "vsumq" 5 undefined)
               ,("vldwrdSplat",MDCore "vsceg" 3 undefined)
               ,("vlwrdSplat",MDCore "vscef" 3 undefined)
               ,("vlvgp",MDCore "vlvgp" 3 (const VRRf))
               ,("vgm",MDCore "vgm" 3 undefined)
               ,("vByteMask",MDCore "vgbm" 3 undefined)
               ,("vgbm",MDCore "vgbm" 3 undefined)
               ,("vgefMR",MDCore "vgef" 3 undefined)
               ,("vgegMR",MDCore "vgeg" 3 undefined)
               ,("vgegAll",MDCore "vgeg" 3 undefined)
               ,("vlr",MDCore "vlr" 5 (const VRRa))
               ,("movVR",MDCore "vlr" 5 (const VRRa))
               ,("movGPR",undefined)
               ,("vlgv",MDCore "vlgv" 5 (\[m4, _] -> VRSc (Just m4) False))
               -- TODO: figure out where the constant zero GPR goes to
               ,("vlgv0",MDCore "vlgv" 5 (\[m4, _] -> VRSc (Just m4) True))
               ,("slgfi",MDCore "slgfi" 5 (const RILa))
               ,("vpk",MDCore "vpk" 3 undefined)
               ,("vpks",MDCore "vpks" 3 undefined)
               ,("vpkls",MDCore "vpkls" 3 undefined)
               ,("vuph",MDCore "vuph" 3 undefined)
               ,("vuplh",MDCore "vuplh" 3 undefined)
               ,("vupl",MDCore "vupl" 3 undefined)
               ,("vupll",MDCore "vupll" 3 undefined)
               ,("vrep",MDCore "vrep" 3 undefined)
               ,("vrepib",MDCore "vrepi" 3 undefined) --TODO: IBM documentation only has vrepi
               ,("vmsl",MDCore "vmsl" 3 undefined)
               ,("vmslg",MDCore "vmsl" 3 undefined) -- TODO: IBM documentation only has vmsl
               ,("jump",MDCore "j" 3 (const JumpUnconditional))
               ,("branchImmNotEq",MDCore "cgijne" 3 (const CmpImmBranch))
               ,("branchImmNotHigh",MDCore "cgijnh" 3 (const CmpImmBranch))
               ,("branchNotLow",MDCore "cgrjnl" 3 (const CmpBranch))
               ,("branchLow",MDCore "cgrjl" 3 (const CmpBranch))
               ,("cgijne",MDCore "cgijne" 3 (const CmpImmBranch))
               ,("cgijnh",MDCore "cgijnh" 3 (const CmpImmBranch))
               ,("cgrjnl",MDCore "cgrjnl" 3 (const CmpBranch))
               -- TODO make sure this is correct
               ,("vftci",MDCore "vftci" 3 (const $ VRIe Nothing Nothing))
               -- TODO double check vfpsodb is what we want
               ,("vfpso",MDCore "vfpsodb" 3 (const $ VRRa))
               -- Spilling
               ,("spillGPR",MDCore "std" 5 (const $ LabelIncrFormat))
               ,("despillGPR",MDCore "lg" 5 (const $ LabelIncrFormat))
               ,("spillVR",MDCore "vst" 5 (const $ LabelIncrFormat))
               ,("despillVR",MDCore "vl" 5 (const $ LabelIncrFormat))
               ,("moduloDWLoad",MDCore "lg" 5 (const VRXP))
               ,("moduloDWStore",MDCore "stg" 5 (const VRXP))
               ,("moduloVLoad",MDCore "vl" 5 (const VRXP))
               ,("moduloVStore",MDCore "vst" 5 (const VRXP))
               ,("moduloVLoadG",MDCore "vl" 5 (const $ VRX Nothing False False))
               ,("moduloVStoreG",MDCore "vst" 5 (const $ VRX Nothing False False))
               -- FIXME fill in rest of MetaData
               ]
