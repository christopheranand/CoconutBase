-- |
-- Module      :  Coconut.Core.Printer
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a functionality for printing the @CoreISA@ DSL in it's assembly format
-- (i.e,. for the purposes of code generation)
{-# LANGUAGE OverloadedStrings #-}

module Coconut.Core.Printer where

import Data.Word (Word64)
import qualified Numeric

import Coconut.BaseTypes
import Coconut.Core.CoreISA
import Coconut.Core.CoreHardware
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (elemIndex)
import Data.Map.Strict ((!))

-- TODO port this code to seperate Printer module and use this more dynamic definition for printConstantReg
-- printConstantReg = case constantReg of
--                      (t,v) -> regPrefix t <> vf
printConstantReg = "R6"

-- NOTE on adding constant loads:
--    1) lookup immediate (first convert the [Int] to (Word64,Word64)) in cTable
--    2) the displacement will be the index into cTable multiplied by 16
--    3) the resulting load will be vl V*,displacement(R6,R0) where you hardcode R6,R0
instructionFormatPrint :: Maybe (InstructionFormat CORE) -> ByteString -> [(RegType CORE, ByteString)] -> [Int] -> Maybe String -> [(Word64,Word64)] -> ByteString
instructionFormatPrint (Just RRFa) name [(type0, r0), (type1, r1), (type2, r2)] [] label cTable
  = name <> " " <> regPrefix type2 <> r2 <> "," <> regPrefix type0 <> r0 <> "," <> regPrefix type1 <> r1
instructionFormatPrint (Just RRE) name [(type0, r0), (type1, r1)] [] label cTable
  = name <> " " <> regPrefix type0 <> r0 <> "," <> regPrefix type1 <> r1
-- NOTE: Extra output condition register which should just be discarded  (cgr)
instructionFormatPrint (Just RRE) name [(type0, r0), (type1, r1), _] [] label cTable
  = name <> " " <> regPrefix type0 <> r0 <> "," <> regPrefix type1 <> r1
instructionFormatPrint (Just RR) name [(type0, r0), (type1, r1)] [] label cTable
  = name <> " " <> regPrefix type0 <> r0 <> "," <> regPrefix type1 <> r1
instructionFormatPrint (Just RXa) name [(type0, x2), (type1, b2), (type2, r1)] [d2] label cTable
  = name <> " " <> regPrefix type2 <> r1 <> "," <> toBS d2 <> "(" <> regPrefix type0 <> x2 <> "," <> regPrefix type1 <> b2 <> ")"
instructionFormatPrint (Just RILa) name [(type0, r1),_] [i2] label cTable
  = name <> " " <> regPrefix type0 <> r1 <> "," <> toBS i2
instructionFormatPrint (Just RIc) name [(type0, r1)] [m1] label cTable
  = name <> " " <> toBS m1 <> "," <> regPrefix type0 <> r1
instructionFormatPrint (Just RSI) name [(type0, r1), (type1, r3)] [ri2] label cTable
  = name <> " " <> regPrefix type0 <> r1 <> "," <> regPrefix type1 <> r3 <> "," <> toBS ri2
-- NOTE this was hardcoded to use a label instead of immediate
instructionFormatPrint (Just RILb) name [(type0, r1)] [] mLabel cTable
  = case mLabel of
      Just "SCRATCH" -> "la " <> regPrefix type0 <> r1 <> ",SCRATCH" -- TODO should add alternative initMR instead of this
      Just label -> name <> " " <> regPrefix type0 <> r1 <> "," <> BS.pack label
      Nothing -> error "instructionFormatPrint RILb given Nothing for label"

-- correct
{- instructionFormatPrint (Just RXYa) name [(type0, x2), (type1, b2), (type2, r1)] [d2]
  = name <> " " <> regPrefix type2 <> r1 <> "," <> toBS d2 <> "(" <> regPrefix type0 <> x2 <> "," <> regPrefix type1 <> b2 <> ")" -}

-- x2 and b2 are set to default registers, change this later
-- NOTE: The last (MR s) argument is discarded, presumably because
-- we don't care about the "modified" memory region in codegen
instructionFormatPrint (Just RXYa) name [(type0, b2), (type1, x2), (type2, r1), _] [d2] label cTable
  = name <> " " <> regPrefix type2 <> r1 <> "," <> toBS d2
         <> "(" <> regPrefix type0 <> b2 <> "," <> regPrefix type1 <> x2 <> ")"
-- NOTE: This is a special case for subfG, where the displacement immediate is 0
instructionFormatPrint (Just RXYa) name [(type0, b2), (type1, x2), (type2, r1)] [] label cTable
  = name <> " " <> regPrefix type2 <> r1 <> "," <> toBS 0
         <> "(" <> regPrefix type0 <> b2 <> "," <> regPrefix type1 <> x2 <> ")"
-- TODO: look into VRRb because Lucas had doubts - might need to add Maybe Int (possible 3rd arg) instead of Int - possible square bracket
-- around second int?
instructionFormatPrint (Just (VRRb m4 m5 withBrackets)) name [(type0, v2), (type1, v3), (type2, v1)] imm label cTable
  = name <> " " <> regPrefix type2 <> v1 <> "," <> regPrefix type0 <> v2 <> "," <> regPrefix type1 <> v3
         <> printMask m4 <> printMaskWithBrackets m5 withBrackets
-- TODO: look into VRRc because Lucas had doubts - might need to add Maybe Int instead of Int
instructionFormatPrint (Just (VRRc m4 m5 m6)) name [(type0, v2), (type1, v3), (type2, v1)] imm label cTable
  = name <> " " <> regPrefix type2 <> v1 <> "," <> regPrefix type0 <> v2 <> "," <> regPrefix type1 <> v3
         <> printMask m4 <> printMask m5 <> printMask m6
-- NOTE: Since this is the instruction format corresponding to "verim", the assumption is that the
-- first register value is being overwritten, hence we ignore that in the register list
-- FIXME are these the correct registers as assigned by op_o3 in CodeGraph.verim?
instructionFormatPrint (Just (VRId m5)) name regs@[(typeN,v0), (type0, v2), (type1, v3), (type2, v1)] [i4,x] label cTable
  = name <> " " <> regPrefix type2 <> v1 <> "," <> regPrefix type0 <> v2
         <> "," <> regPrefix type1 <> v3 <> "," <> toBS i4 <> "," <> toBS x <> " * " <> (BS.pack $ show regs)
instructionFormatPrint (Just (VRId m5)) name regs@[(type0, v2), (type1, v3), (type2, v1)] [i4] label cTable
  = name <> " " <> regPrefix type2 <> v1 <> "," <> regPrefix type0 <> v2
         <> "," <> regPrefix type1 <> v3 <> "," <> toBS i4
-- TODO check VRIe is correct for vftci
instructionFormatPrint (Just (VRIe _ _)) name regs@[(type0, v0), (type1, v1)] [i3,i4,i5] label cTable
  = name <> " " <> regPrefix type1 <> v1 <> "," <> regPrefix type0 <> v0
         <> toBS i3 <> "," <> toBS i4 <> "," <> toBS i5
instructionFormatPrint (Just (VRX m3 withBrackets False)) name [(type0, x2), (type1, b2), (type2, v1), _] [d2] label cTable
  = name <> " " <> regPrefix type2 <> v1 <> "," <> toBS d2 <> "(" <> regPrefix type0 <> x2 <> "," <> regPrefix type1 <> b2 <> ")"
         <> printMaskWithBrackets m3 withBrackets
instructionFormatPrint (Just (VRX m3 withBrackets True)) name [(type0, x2), (type2, v1), _] [d2] label cTable
  = name <> " " <> regPrefix type2 <> v1 <> "," <> toBS d2 <> "(" <> regPrefix type0 <> x2 <> "," <> "R0" <> ")"
         <> printMaskWithBrackets m3 withBrackets
instructionFormatPrint (Just VRXP) name [(type0, x2), (type1, b2), (type2, v1), _] [d2] label cTable
  = name <> " " <> regPrefix type2 <> v1 <> "," <> toBS d2 <> "(" <> regPrefix type0 <> b2 <> "," <> "R0" <> ")"
instructionFormatPrint (Just (VRSa m4 False)) name [(type0, v3), (type1, b2), (type2, v1)] [_, d2] label cTable
  = name <> " " <> regPrefix type2 <> v1 <> "," <> regPrefix type0 <> v3 <> "," <> toBS d2 <> "(" <> regPrefix type1 <> b2 <> ")"
         <> printMask m4
instructionFormatPrint (Just (VRSa m4 True)) name [(type0, v3), (type2, v1)] [_, d2] label cTable
  = name <> " " <> regPrefix type2 <> v1 <> "," <> regPrefix type0 <> v3 <> "," <> toBS d2 <> "(R0)"
         <> printMask m4
instructionFormatPrint (Just (VRSc m4 False)) name [(type0, v3), (type1, b2), (type2, r1)] [_, d2] label cTable
  = name <> " " <> regPrefix type2 <> r1 <> "," <> regPrefix type0 <> v3 <> "," <> toBS d2 <> "(" <> regPrefix type1 <> b2 <> ")"
         <> printMask m4
instructionFormatPrint (Just (VRSc m4 True)) name [(type0, v3), (type2, r1)] [_, d2] label cTable
  = name <> " " <> regPrefix type2 <> r1 <> "," <> regPrefix type0 <> v3 <> "," <> toBS d2 <> "(R0)"
         <> printMask m4
instructionFormatPrint (Just VRRf) name [(type0, r2), (type1, r3), (type2, v1)] [] label cTable
  = name <> " " <> regPrefix type2 <> v1 <> "," <> regPrefix type0 <> r2 <> "," <> regPrefix type1 <> r3
instructionFormatPrint (Just (VRRe m5 m6)) name [(type0, v2), (type1, v3), (type2, v4), (type3, v1)] [] label cTable
  = name <> " " <> regPrefix type3 <> v1 <> "," <> regPrefix type0 <> v2 <> ","
         <> regPrefix type1 <> v3 <> "," <> regPrefix type2 <> v4 <> printMask m5 <> printMask m6
instructionFormatPrint (Just (VRReSWAP m5 m6)) name [(type0, v2), (type1, v3), (type2, v4), (type3, v1)] [] label cTable
  = name <> " " <> regPrefix type3 <> v1 <> "," <> regPrefix type0 <> v3 <> ","
         <> regPrefix type1 <> v2 <> "," <> regPrefix type2 <> v4 <> printMask m5 <> printMask m6
instructionFormatPrint (Just (VRRa)) name [(type0, v0), (type1, v1)] [] label cTable
  = name <> " " <> regPrefix type1 <> v1 <> "," <> regPrefix type0 <> v0
instructionFormatPrint (Just (VRRa)) name [(type0, v0), (type1, v1)] [sb] label cTable
  = name <> " " <> regPrefix type1 <> v1 <> "," <> regPrefix type0 <> v0 <> "," <> toBS sb
instructionFormatPrint (Just ConstDecodeVR) name [(type0, v)] [imm1, imm2] label cTable = let
  mDisp = elemIndex (fromIntegral imm1, fromIntegral imm2) cTable
  in case mDisp of
    Nothing -> error $ "instructionFormatPrint on " ++ show name ++ " can't find immediate" ++ show (imm1,imm2)
    Just idx -> "vl" <> " " <> regPrefix type0 <> v <> "," <> toBS (idx * 16)
                     <> "(" <> printConstantReg <> ",R0)"
instructionFormatPrint (Just ConstDecodeGPR) name [(type0, g)] [imm] label cTable = let
  mDisp = elemIndex (fromIntegral imm, fromIntegral imm) cTable
  in case mDisp of
    Nothing -> error $ "instructionFormatPrint on " ++ show name ++ " can't find immediate" ++ show imm
                      ++"\n\ncTable: " ++ show cTable
    Just idx -> "lg" <> " " <> regPrefix type0 <> g <> "," <> toBS (idx * 16)
                     <> "(" <> printConstantReg <> ",R0)"
-- Reference: IBM z/Architecture Reference Summary, p.54 on
-- Extended-Mnemonic Instructions for relative-branch instructions
-- NOTE: for now just hardcode "j" instruction
instructionFormatPrint (Just JumpUnconditional) name _ _ (Just label) _ =
  "j" <> " " <> BS.pack label
instructionFormatPrint (Just JumpConditional) name _ _ (Just label) _ =
  name <> " " <> BS.pack label
instructionFormatPrint (Just CmpImmBranch) name [(type0, r0)] [imm] (Just label) _ =
  name <> " " <> regPrefix type0 <> r0 <> "," <> toBS imm <> "," <> BS.pack label
instructionFormatPrint (Just CmpBranch) name [(type0, r0),(type1,r1)] [] (Just label) _ =
  name <> " " <> regPrefix type0 <> r0 <> "," <> regPrefix type1 <> r1 <> "," <> BS.pack label
instructionFormatPrint (Just LabelIncrFormat) name [(type0,r0)] [imm] (Just label) _ =
  -- Non-FIFO Spilling
  name <> " " <> regPrefix type0 <> r0 <> "," <> (BS.pack label) <> "+" <> (BS.pack $ show $ imm * 16)
instructionFormatPrint (Just LabelIncrFormat) name [(type0,r0),(type1,r1)] [imm] (Just label) _ =
  -- FIFO Spilling
  name <> " " <> regPrefix type0 <> r0 <> "," <> (BS.pack label) <> "+" <> (BS.pack $ show $ imm * 16)
  <> "(" <> regPrefix type1 <> r1 <> ")"
instructionFormatPrint (Just TODO) name _ _ label cTable
  = "TODO: " <> name
instructionFormatPrint Nothing _ _ _ _ _ = ""
-- TODO: better error handling? We will want to see length of arg/imm lists
instructionFormatPrint _ name regList immList label cTable =
  "error in instructionFormatPrint: " <> name <> " where\n\t regList = " <> toBS (show regList)
                                              <> "\n\t immList = " <> toBS (show immList)


-- TODO: flag ordering needs to be fixed e.g. m5 and m6

printMask :: Maybe Int -> ByteString
printMask (Just m) = "," <> (BS.pack $ show m)
printMask (Nothing) = ""

printMaskWithBrackets :: Maybe Int -> Bool -> ByteString
printMaskWithBrackets m True  = "[" <> printMask m <> "]"
printMaskWithBrackets m False = printMask m

toBS :: (Show a) => a -> ByteString
toBS = BS.pack . show


instance Printer CORE where
  data InstructionFormat CORE =
    RRFa | RRE | RR | RXa | RILa | RIc | RSI | RILb | RXYa
    | VRRb { m4 :: Maybe Int, m5 :: Maybe Int, withBrackets :: Bool }
    | VRRc { m4 :: Maybe Int, m5 :: Maybe Int, m6 :: Maybe Int }
    | VRId { m5 :: Maybe Int }
    | VRIe { m4 :: Maybe Int, m5 :: Maybe Int }
    | VRX  { m3 :: Maybe Int, withBrackets :: Bool, constantReg :: Bool }
    | VRXP
    | VRSa { m4 :: Maybe Int, constantReg :: Bool }
    | VRSc { m4 :: Maybe Int, constantReg :: Bool }
    | VRRf
    | VRRa
    | VRRe { m5 :: Maybe Int, m6 :: Maybe Int }
    -- FIXME we added VRReSWAP just to flip around the selb arguments, arguably this should be changed
    -- in the DSL (i.e. in zMASS etc) instead
    | VRReSWAP { m5 :: Maybe Int, m6 :: Maybe Int }
    | TODO
    | ConstDecodeVR
    | ConstDecodeGPR
    | JumpUnconditional
    | JumpConditional
    | CmpImmBranch
    | CmpBranch
    | LabelIncrFormat -- TODO implemented for spilling, should find proper format

  printableInstrLabel = mdCorePName
  printableInstrFormat = mdFormatConv
  printableInstruction metaData format name args imms label cTable =
    instructionFormatPrint format name args imms label cTable
  printableTable _cg label pairs =
    let
      header = BS.pack label <> "   DS   0L"
      genHex x = let h = Numeric.showHex x "" in "0x" <> (replicate (16-length h)'0') <> h
      genPair (a,b) = ["    DC    XL8'" <> BS.pack (Numeric.showHex a "") <> "'"
                      , "    DC    XL8'" <> BS.pack (Numeric.showHex b "") <> "'"]
    in header : concatMap genPair pairs
  printableSectionLabel _cg label = BS.pack label <> " DS 0H"

-- TODO is Coconut.Core.Printer module needed anymore?
-- Helper functions
-- argIntersperse :: [String] -> String
-- argIntersperse []     = ""
-- argIntersperse (v:vs)
--   | v == "" = argIntersperse vs
--   | otherwise = " " ++ v ++ argIntersperse vs

-- argIntersperseCommas :: [String] -> String
-- argIntersperseCommas [] = "()"
-- argIntersperseCommas (v:vs) =
--   let
--     intersperse' [] = ""
--     intersperse' (v':vs')
--       | v' == "" = intersperse' vs'
--       | otherwise = ", " ++ v' ++ intersperse' vs'
--   in
--     case v of
--       "" -> argIntersperseCommas vs
--       _ ->
--         "(" ++ v ++ intersperse' vs ++ ")"


-- printNoImmsG :: String -> Print a
-- printNoImmsG name = Print ( \ vs -> name ++ argIntersperseCommas (map gprTag vs ) )

-- printNoImmsV :: String -> Print a
-- printNoImmsV name = Print ( \ vs -> name ++ argIntersperseCommas (map vrTag vs ) )

-- -- TODO find a better way to tag v or r infront of vr's / gpr's respectively
-- vrTag vNum = 'v':vNum
-- gprTag gNum = 'r':gNum

-- instance CoreISA Print where
--   -- TODO implement the rest of CoreISA Printer instance
--   -- Getting values into GPRs
--   unintegerG imm = Print ( \ (v : _) -> "unintegerG" ++ argIntersperseCommas [show imm, gprTag v] )
--   unwrd64 imm    = Print ( \ (v : _) -> "unwrd64" ++ argIntersperseCommas [show imm, gprTag v] )
--   unwrdsG imm    = Print ( \ (v : _) -> "unwrdsG" ++ argIntersperseCommas [show imm, gprTag v] )

--   -- Getting values into VRs
--   uninteger imm  = Print ( \ (v : _) -> "uninteger" ++ argIntersperseCommas [show imm, vrTag v] )
--   unintegerS imm = Print ( \ (v : _) -> "unintegerS" ++ argIntersperseCommas [show imm, vrTag v] )
--   undwrds imm    = Print ( \ (v : _) -> "undwrds" ++ argIntersperseCommas [show imm, vrTag v] )
--   unwrds imm     = Print ( \ (v : _) -> "unwrds" ++ argIntersperseCommas [show imm, vrTag v] )
--   unshorts imm   = Print ( \ (v : _) -> "unshorts" ++ argIntersperseCommas [show imm, vrTag v] )
--   unbytes imm    = Print ( \ (v : _) -> "unbytes" ++ argIntersperseCommas [show imm, vrTag v] )
--   unnibbles imm  = Print ( \ (v : _) -> "unnibbles" ++ argIntersperseCommas [show imm, vrTag v] )
--   unbits imm     = Print ( \ (v : _) -> "unbits" ++ argIntersperseCommas [show imm, vrTag v] )
--   unfloats imm   = Print ( \ (v : _) -> "unfloats" ++ argIntersperseCommas [show imm, vrTag v] )
--   undoubles imm  = Print ( \ (v : _) -> "undoubles" ++ argIntersperseCommas [show imm, vrTag v] )

--   -- Retrieving values from GPRs and VRs. Should not be used.
--   unsignedG = error "unsignedG"
--   signedG   = error "signedG"
--   wrd64     = error "wrd64"
--   wrdsG     = error "wrdsG"
--   integer   = error "integer"
--   integerS  = error "integerS"
--   hexval    = error "hexval"
--   dwrds     = error "dwrds"
--   wrds      = error "wrds"
--   shorts    = error "shorts"
--   bytes     = error "bytes"
--   nibbles   = error "nibbles"
--   bits      = error "bits"
--   floats    = error "floats"
--   doubles   = error "doubles"

--   -- GPR logicals
--   andG _ _  = printNoImmsG "andG"
--   andcG _ _ = printNoImmsG "andcG"
--   orG _ _   = printNoImmsG "orG"
--   orcG _ _  = printNoImmsG "orcG"
--   xorG _ _  = printNoImmsG "xorG"
--   xorcG _ _ = printNoImmsG "xorcG"
--   nandG _ _ = printNoImmsG "nandG"
--   norG _ _  = printNoImmsG "norG"
--   eqvG _ _  = printNoImmsG "eqvG"

--   -- Other common GPR instructions
--   addG _ _             = printNoImmsG "addG"
--   subfG _ _            = printNoImmsG "subfG"
--   negG _               = printNoImmsG "negG"
--   mulldG _ _           = printNoImmsG "mulldG"
--   mulhdG _ _           = printNoImmsG "mulhdG"
--   mulhduG _ _          = printNoImmsG "mulhduG"
--   divdG _ _            = printNoImmsG "divdG"
--   divduG _ _           = printNoImmsG "divduG"
--   sldG _ _             = printNoImmsG "sldG"
--   slwG _ _             = printNoImmsG "slwG"
--   sradG _ _            = printNoImmsG "sradG"
--   sradiG _ imm         = Print ( \ (v : vs) -> "sradiG" ++ argIntersperseCommas ([gprTag v, show imm]
--                                                                                  ++ map gprTag vs ) )
--   srawG _ _            = printNoImmsG "srawG"
--   srawiG _ imm         = Print ( \ (v : vs) -> "srawiG" ++ argIntersperseCommas ([gprTag v, show imm]
--                                                                                  ++ map gprTag vs) )
--   srdG _ _             = printNoImmsG "srdG"
--   srwG _ _             = printNoImmsG "srwG"
--   rldiclG _ sh mb      = Print ( \ (v : vs) -> "rldiclG" ++ argIntersperseCommas ([gprTag v, show sh, show mb] ++ map gprTag vs) )
--   rldicrG _ sh me      = Print ( \ (v : vs) -> "rldiclG" ++ argIntersperseCommas ([gprTag v, show sh, show me] ++ map gprTag vs) )
--   rldimiG _ _ sh mb    = Print ( \ (v1 : v2 : vs) -> "rldimiG" ++ argIntersperseCommas ([gprTag v1, gprTag v2, show sh, show mb] ++ map gprTag vs) )
--   rldicG _ sh mb       = Print ( \ (v : vs) -> "rldicG" ++ argIntersperseCommas ([gprTag v, show sh, show mb] ++ map gprTag vs) )
--   rldclG _ _ imm       = Print ( \ (v1 : v2 : vs) -> "rldclG" ++ argIntersperseCommas ([gprTag v1, gprTag v2, show imm] ++ map gprTag vs) )
--   rldcrG _ _ imm       = Print ( \ (v1 : v2 : vs) -> "rldcrG" ++ argIntersperseCommas ([gprTag v1, gprTag v2, show imm] ++ map gprTag vs) )
--   rlwimiG _ _ sh mb me = Print ( \ (v1 : v2 : vs) -> "rlwimiG" ++ argIntersperseCommas ([gprTag v1, gprTag v2, show sh, show mb, show me] ++ map gprTag vs) )

--   -- Vector instructions
--   vac size _ _ _            = Print ( \ vs -> "vac" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vacc size _ _             = Print ( \ vs -> "vacc" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vaccc size _ _ _          = Print ( \ vs -> "vaccc" ++ argIntersperseCommas (show size : map vrTag vs) )
--   va size _ _               = Print ( \ vs -> "va" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vsum size _ _             = Print ( \ vs -> "vsum" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vsumq size _ _            = Print ( \ vs -> "vsumq" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vs size _ _               = Print ( \ vs -> "vs" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vsbi size _ _ _           = Print ( \ vs -> "vsbi" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vscbi size _ _            = Print ( \ vs -> "vscbi" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vsbcbi size _ _ _         = Print ( \ vs -> "vsbcbi" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vmah size _ _ _           = Print ( \ vs -> "vmah" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vmalh size _ _ _          = Print ( \ vs -> "vmalh" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vmal size _ _ _           = Print ( \ vs -> "vmal" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vml size _ _              = Print ( \ vs -> "vml" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vmh size _ _              = Print ( \ vs -> "vmh" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vmlh size _ _             = Print ( \ vs -> "vmlh" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vesrl0 size _ sh          = Print ( \ (v : vs) -> "vesrl0" ++ argIntersperseCommas ([show size, vrTag v, show sh] ++ map vrTag vs) )
--   vesra0 size _ sh          = Print ( \ (v : vs) -> "vesra0" ++ argIntersperseCommas ([show size, vrTag v, show sh] ++ map vrTag vs) )
--   vesl0 size _ sh           = Print ( \ (v : vs) -> "vesl0" ++ argIntersperseCommas ([show size, vrTag v, show sh] ++ map vrTag vs) )
--   vsldb imm _ _             = Print ( \ vs -> "vsldb" ++ argIntersperseCommas (show imm : map vrTag vs) )
--   vsl _ _                   = printNoImmsV "vsl"
--   vsrl _ _                  = printNoImmsV "vsrl"
--   vldwrdSplat imm1 _ imm2 _ = Print ( \ (v : vs) -> "vldwrdSplat" ++ argIntersperseCommas ([show imm1, vrTag v, show imm2] ++ map vrTag vs) )
--   vlwrdSplat imm1 _ imm2 _  = Print ( \ (v : vs) -> "vlwrdSplat" ++ argIntersperseCommas ([show imm1, vrTag v, show imm2] ++ map vrTag vs) )
--   selb _ _ _                = printNoImmsV "selb"
--   shufb _ _ _               = printNoImmsV "shufb"
--   vbperm _ _                = printNoImmsV "vbperm"
--   vmrh size _ _             = Print ( \ vs -> "vmrh" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vmrl size _ _             = Print ( \ vs -> "vmrl" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vpk size _ _              = Print ( \ vs -> "vpk" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vpks size _ _             = Print ( \ vs -> "vpks" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vpkls size _ _            = Print ( \ vs -> "vpkls" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vuph size _ _             = Print ( \ vs -> "vuph" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vuplh size _ _            = Print ( \ vs -> "vuplh" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vupl size _ _             = Print ( \ vs -> "vupl" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vupll size _ _            = Print ( \ vs -> "vupll" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vmsl size _ _ _ imm       = Print ( \ (v1 : v2 : v3 : vs) -> "vmsl" ++ argIntersperseCommas ([show size,vrTag v1,vrTag v2,vrTag v3, show imm] ++ map vrTag vs) )
--   vmslg = vmsl 3
--   vlc imm _                 = Print ( \ vs -> "vlc" ++ argIntersperseCommas (show imm : map vrTag vs) )
--   vlp imm _                 = Print ( \ vs -> "vlp" ++ argIntersperseCommas (show imm : map vrTag vs) )

--   -- Vector logicals
--   vnot _    = Print ( \ vs -> "vnot" ++ argIntersperseCommas (map vrTag vs ) )
--   vand _ _  = Print ( \ vs -> "vand" ++ argIntersperseCommas (map vrTag vs ) )
--   vandc _ _ = Print ( \ vs -> "vandc" ++ argIntersperseCommas (map vrTag vs ) )
--   vor _ _   = Print ( \ vs -> "vor" ++ argIntersperseCommas (map vrTag vs ) )
--   vnor _ _  = Print ( \ vs -> "vnor" ++ argIntersperseCommas (map vrTag vs ) )
--   vxor _ _  = Print ( \ vs -> "vxor" ++ argIntersperseCommas (map vrTag vs ) )

--   -- DP ops
--   dfa _ _    = Print ( \ vs -> "vfa" ++ argIntersperseCommas (map vrTag vs ) )
--   dfm _ _    = Print ( \ vs -> "vfm" ++ argIntersperseCommas (map vrTag vs ) )
--   dfma _ _ _ = Print ( \ vs -> "vfma" ++ argIntersperseCommas (map vrTag vs ) )
--   dfms _ _ _ = Print ( \ vs -> "vfms" ++ argIntersperseCommas (map vrTag vs ) )
--   dfs _ _    = Print ( \ vs -> "vfs" ++ argIntersperseCommas (map vrTag vs ) )
--   dfdiv _ _  = Print ( \ vs -> "vfd" ++ argIntersperseCommas (map vrTag vs ) )
--   dfsqrt _   = Print ( \ vs -> "dfsqrt" ++ argIntersperseCommas (map vrTag vs ) )

--   -- Selected vector integer instructions
--   vfch _ _               = printNoImmsV "vfch"
--   vfche _ _              = printNoImmsV "vfche"
--   vfce _ _               = printNoImmsV "vfce"
--   vceq size _ _          = Print ( \ vs -> "vceq" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vch size _ _           = Print ( \ vs -> "vch" ++ argIntersperseCommas (show size : map vrTag vs) )
--   vchl size _ _          = Print ( \ vs -> "vchl" ++ argIntersperseCommas (show size : map vrTag vs) )
--   verim pow sh _ _ _     = Print ( \ vs -> "verim" ++ argIntersperseCommas ([show pow, show sh] ++ map vrTag vs) )
--   -- What does this actually do?
--   verimD _x pow sh _ _ _ = Print ( \ vs -> "verimD" ++ argIntersperseCommas ([show pow, show sh] ++ map vrTag vs) )
--   vdwrdIdx imm _ _       = Print ( \ vs -> "vdwrdIdx" ++ argIntersperseCommas (show imm : map vrTag vs))
--   vgm imm elemSize       = Print ( \ (v : _) -> "vgm" ++ argIntersperseCommas [show imm, show elemSize, vrTag v] )
--   -- TODO what are flags
--   vByteMask flags        = Print ( \ (v : _) -> "vByteMask" ++ argIntersperseCommas [show flags, vrTag v] )
--   vgbm mask              = Print ( \ (v : _) -> "vgbm" ++ argIntersperseCommas [show mask, vrTag v] )
--   vslb _ _               = Print ( \ vs -> "vslb" ++ argIntersperseCommas (map vrTag vs ) )
--   vsrlb _ _              = Print ( \ vs -> "vsrlb" ++ argIntersperseCommas (map vrTag vs ) )
--   verll size _ imm _     = Print ( \ (v : g : vs) -> "verll" ++ argIntersperseCommas ([show size, vrTag v, show imm, gprTag g] ++ map vrTag vs) )
--   vesrl size _ imm _     = Print ( \ (v : g : vs) -> "vesrl" ++ argIntersperseCommas ([show size, vrTag v, show imm, gprTag g] ++ map vrTag vs) )
--   vesra imm1 _ imm2 _    = Print ( \ (v : g : vs) -> "vesra" ++ argIntersperseCommas ([show imm1, vrTag v, show imm2, gprTag g] ++ map vrTag vs) )
--   vesl imm1 _ imm2 _     = Print ( \ (v : g : vs) -> "vesl" ++ argIntersperseCommas ([show imm1, vrTag v, show imm2, gprTag g] ++ map vrTag vs) )


--   -- Power load/store
--   vlr _            = Print ( \ vs -> "vlr" ++ argIntersperseCommas (map vrTag vs) )
--   ldxMR _ imm _    = (Print ( \ (v : vs) -> "ldxMR" ++ argIntersperseCommas (gprTag v : show imm : (map gprTag vs ))), Print (const "MR Undefined") )
--   initMR name _    = Print ( \ vs -> "initMR" ++ argIntersperseCommas (name : vs) )
--   incMR _ imm      = Print ( \ vs -> "incMR" ++ argIntersperseCommas (vs ++ [show imm]) )
--   stdMR _ imm _    = Print ( \ (v : vs) -> "stdMR" ++ argIntersperseCommas (gprTag v : show imm : map gprTag vs) )
--   stwMR _ imm _    = Print ( \ (v : vs) -> "stwMR" ++ argIntersperseCommas (gprTag v : show imm : map gprTag vs) )
--   stdxMR _ imm _ _ = Print ( \ (v : vs) -> "stdxMR" ++ argIntersperseCommas (gprTag v : show imm : map gprTag vs) )
--   stvxMR _ imm _ _ = Print ( \ (v : vs) -> "stvxMR" ++ argIntersperseCommas (gprTag v : show imm : map vrTag vs) )

--   -- Z load/store
--   lgMR _ imm _          = (Print ( \ (v : vs) -> "lgMR" ++ argIntersperseCommas (gprTag v : show imm : map gprTag vs)), Print (const "MR Undefined") )
--   vldxMR _ imm _        = (Print ( \ (v : vs) -> "vldxMR" ++ argIntersperseCommas (gprTag v : show imm : map vrTag vs)), Print (const "MR Undefined") )
--   vld0MR _ imm          = (Print ( \ (v : vs) -> "vld0MR" ++ argIntersperseCommas (vrTag v : show imm : map vrTag vs)), Print (const "MR Undefined") )
--   vldInMR _ imm _       = (Print ( \ (v : vs) -> "vldInMR" ++ argIntersperseCommas (vrTag v : show imm : map vrTag vs)), Print (const "MR Undefined") )
--   fauxDep _ _ _ _       = Print ( \ vs -> "fauxDep" ++ argIntersperseCommas (map vrTag vs) )
--   vgefMR eIdx _ _ imm _ = (Print ( \ (v1 : v2 : vs) -> "vgefMR" ++ argIntersperseCommas (show eIdx : vrTag v1 : vrTag v2 : show imm : map vrTag vs)), Print (const "MR Undefined") )
--   vgegMR eIdx _ _ imm _ = (Print ( \ (v1 : v2 : vs) -> "vgegMR" ++ argIntersperseCommas (show eIdx : vrTag v1 : vrTag v2 : show imm : map vrTag vs)), Print (const "MR Undefined") )
--   vgegAll imm _ _       = (Print ( \ vs -> "vgegAll" ++ argIntersperseCommas (show imm : map vrTag vs)), Print (const "MR Undefined") )
--   vleib eIdx mask _     = Print ( \ vs -> "vleib" ++ argIntersperseCommas ([show eIdx, show mask] ++ map vrTag vs) )
--   vleih eIdx mask _     = Print ( \ vs -> "vleih" ++ argIntersperseCommas ([show eIdx, show mask] ++ map vrTag vs) )
--   vleif eIdx mask _     = Print ( \ vs -> "vleif" ++ argIntersperseCommas ([show eIdx, show mask] ++ map vrTag vs) )
--   vleig eIdx mask _     = Print ( \ vs -> "vleig" ++ argIntersperseCommas ([show eIdx, show mask] ++ map vrTag vs) )
--   vleb eIdx _ imm _ _   = (Print ( \ (v : vs) -> "vleb" ++ argIntersperseCommas ([show eIdx, gprTag v, show imm] ++ map vrTag vs)), Print (const "MR Undefined") )
--   vleh eIdx _ imm _ _   = (Print ( \ (v : vs) -> "vleh" ++ argIntersperseCommas ([show eIdx, gprTag v, show imm] ++ map vrTag vs)), Print (const "MR Undefined") )
--   vlef eIdx _ imm _ _   = (Print ( \ (v : vs) -> "vlef" ++ argIntersperseCommas ([show eIdx, gprTag v, show imm] ++ map vrTag vs)), Print (const "MR Undefined") )
--   vleg eIdx _ imm _ _   = (Print ( \ (v : vs) -> "vleg" ++ argIntersperseCommas ([show eIdx, gprTag v, show imm] ++ map vrTag vs)), Print (const "MR Undefined") )
--   vlegAll _ _ _         = (Print (\ (g0:g1:vs) -> "vlegAll" ++ argIntersperseCommas (gprTag g0:gprTag g1: map vrTag vs )), Print (const "MR Undefined") )
--   verll0 size _ imm     = Print ( \ (v : vs) -> "verll0" ++ argIntersperseCommas ([show size, vrTag v, show imm] ++ map vrTag vs) )
--   vrepib imm            = Print ( \ (v : _) -> "vrepib" ++ argIntersperseCommas [show imm, vrTag v] )
--   vrep size imm _       = Print ( \ vs -> "vrep" ++ argIntersperseCommas ([show size, show imm] ++ map vrTag vs) )
--   vlgv size _ imm _     = Print ( \ (v : vs) -> "vlgv" ++ argIntersperseCommas ([show size, vrTag v, show imm] ++ map gprTag vs) )
--   vlgv0 size _ imm      = Print ( \ (v : vs) -> "vlgv0" ++ argIntersperseCommas ([show size, vrTag v, show imm] ++ map gprTag vs) )
--   vlvgp _ _             = Print ( \ (g0:g1:vs) -> "vlvgp" ++ argIntersperseCommas (gprTag g0:gprTag g1:map vrTag vs ) )

--   -- Misc memory load/store
--   ldMR _ imm     = (Print ( \ (v : vs) -> "ldMR" ++ argIntersperseCommas ([gprTag v, show imm] ++ map gprTag vs)), Print (const "MR Undefined") )
--   lwzMR _ imm    = (Print ( \ (v : vs) -> "lwzMR" ++ argIntersperseCommas ([gprTag v, show imm] ++ map gprTag vs)), Print (const "MR Undefined") )
--   stv0MR _ imm _ = Print ( \ (v : vs) -> "stv0MR" ++ argIntersperseCommas ([vrTag v, show imm] ++ map vrTag vs) )
