{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleContexts,
      OverloadedStrings #-}

module ChaCha where

import Coconut.BaseTypes
import Coconut.Utils.CGWrappers
import Coconut.Core.CoreISA
import Coconut.Core.Interp
import Coconut.Core.CoreHardware
import Coconut.Core.CodeGraph
import Coconut.Graph.CodeGraph
import Coconut.Core.MetaData

import Coconut.RegisterAllocator
import Coconut.Schedule
import Coconut.Graph.Dot
import Coconut.Simulator
import Coconut.CodeGen
import Coconut.HashedSchedule

import qualified Data.Map as Map
import Data.Maybe
import Data.Either
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Bifunctor
import qualified Data.Graph.Inductive.Graph as FGL

import Numeric

defSolver = Glpk
defStages = 2

chaChaInitStateConsts :: forall repr . CoreISA repr => repr VR
chaChaInitStateConsts = unwrds [0x61707865,  0x3320646e,  0x79622d32,  0x6b206574]

chacha20Block :: forall repr . CoreISA repr => (repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
chacha20Block (key0, key1, c_n)
      =
  let

    initState =
      (
        chaChaInitStateConsts,
        key0,
        key1,
        c_n
      )

    newState = inner_block initState

    inner_block state =   roundDiagonal . roundColumn
                        . roundDiagonal . roundColumn
                        . roundDiagonal . roundColumn
                        . roundDiagonal . roundColumn
                        . roundDiagonal . roundColumn
                        . roundDiagonal . roundColumn
                        . roundDiagonal . roundColumn
                        . roundDiagonal . roundColumn
                        . roundDiagonal . roundColumn
                        . roundDiagonal . roundColumn $ state

    addInitState (a, b, c, d) (e, f, g, h) = let
        a' = va 2 a e
        b' = va 2 b f
        c' = va 2 c g
        d' = va 2 d h
      in (a', b', c', d')
  in
    addInitState initState newState

roundColumn :: forall repr . CoreISA repr => (repr VR, repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
roundColumn = quarterRounds

roundDiagonal :: forall repr . CoreISA repr => (repr VR, repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
roundDiagonal (a, b, c, d) = let

  (a', b', c', d') = quarterRounds (
                        a,                -- [ i0,  i1,  i2,  i3]
                        verll128_rot32 b, -- [ i5,  i6,  i7,  i4]
                        verll128_rot64 c, -- [i10, i11,  i8,  i9]
                        verll128_rot96 d  -- [i15, i12, i13, i14]
                    )
  in (
    a',                -- [ i0,  i1,  i2,  i3]
    verll128_rot96 b', -- [ i4,  i5,  i6,  i7]
    verll128_rot64 c', -- [ i8,  i9, i10, i11]
    verll128_rot32 d'  -- [i12, i13, i14, i15]
  )

verll128_rot32 :: forall repr . CoreISA repr => repr VR -> repr VR
verll128_rot32 va = let
  leftShifted  = vslb'  va shiftFourBytes
  rightShifted = vsrlb' va shiftTwelveBytes
  in leftShifted `vxor` rightShifted

verll128_rot64 :: forall repr . CoreISA repr => repr VR -> repr VR
verll128_rot64 va = let
  leftShifted  = vslb'  va shiftEightBytes
  rightShifted = vsrlb' va shiftEightBytes
  in leftShifted `vxor` rightShifted

verll128_rot96 :: forall repr . CoreISA repr => repr VR -> repr VR
verll128_rot96 va = let
  leftShifted  = vslb'  va shiftTwelveBytes
  rightShifted = vsrlb' va shiftFourBytes
  in leftShifted `vxor` rightShifted



-- QUARTER ROUNDS


quarterRoundsHeader :: forall repr . CoreISA repr
  => (repr GPR,repr GPR,repr MR,repr MR,repr GPR,repr GPR)
  -> (repr GPR,repr GPR,repr MR,repr MR,repr GPR,repr GPR)
quarterRoundsHeader (gCnt, gSize, mrIn, mrOut,fif0,fifo1) =
  let
    -- loads
    (v0,mrIn')    = moduloVLoad mrIn    0  gCnt
    (v1,mrIn'')   = moduloVLoad mrIn'   16 gCnt
    (v2,mrIn''')  = moduloVLoad mrIn''  32 gCnt
    (v3,mrIn'''') = moduloVLoad mrIn''' 48 gCnt

    (v4,v5,v6,v7) = quarterRounds (v0, v1, v2, v3)

    -- stores
    mrOut'      = moduloVStore mrOut    0  gCnt v4
    mrOut''     = moduloVStore mrOut'   16 gCnt v5
    mrOut'''    = moduloVStore mrOut''  32 gCnt v6
    mrOut''''   = moduloVStore mrOut''' 48 gCnt v7
  in (gCnt,gSize,mrIn'''',mrOut'''',fif0,fifo1)

quarterRounds :: forall repr . CoreISA repr => (repr VR, repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
quarterRounds (a, b, c, d) =
  let
    (a', b', c', d') =
      (
      quarterRoundOP4 .
      quarterRoundOP3 .
      quarterRoundOP2 .
      quarterRoundOP1
      ) (a, b, c, d)
  in (a', b', c', d')

quarterRoundOP1 :: forall repr . CoreISA repr => (repr VR, repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
quarterRoundOP1 (a, b, c, d) =
  let
    a'  = va 2 a b
    d'  = vxor d a'
    d'' = verll0 2 d' 16
  in (a', b, c, d'')

quarterRoundOP2 :: forall repr . CoreISA repr => (repr VR, repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
quarterRoundOP2 (a, b, c, d) =
  let
    c'  = va 2 c d
    b'  = vxor b c'
    b'' = verll0 2 b' 12
  in (a, b'', c', d)

quarterRoundOP3 :: forall repr . CoreISA repr => (repr VR, repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
quarterRoundOP3 (a, b, c, d) =
  let
    a'  = va 2 a b
    d'  = vxor d a'
    d'' = verll0 2 d' 8
  in (a', b, c, d'')

quarterRoundOP4 :: forall repr . CoreISA repr => (repr VR, repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
quarterRoundOP4 (a, b, c, d) =
  let
    c'  = va 2 c d
    b'  = vxor b c'
    b'' = verll0 2 b' 7
  in (a, b'', c', d)



-- INSTRUCTION ALIASES


vmsl' :: forall repr . CoreISA repr => repr VR -> repr VR -> repr VR -> repr VR
vmsl' va vb vc = vmslg  va vb vc 0

vsrlb' :: forall repr . CoreISA repr => repr VR -> repr VR -> repr VR
vsrlb' va n = vsrlb (vslByteShift n) va

vsrl' :: forall repr . CoreISA repr => repr VR -> repr VR -> repr VR
vsrl' va n = vsrl (vslByteShift n) va

vslb' :: forall repr . CoreISA repr => repr VR -> repr VR -> repr VR
vslb' va n = vslb va (vslByteShift n)

vslByteShift :: forall repr . CoreISA repr => repr VR -> repr VR
vslByteShift va = vslb va shiftToByteSeven


-- CONSTANTS


shiftToByteSeven :: forall repr . CoreISA repr => repr VR
shiftToByteSeven = unbytes [0,0,0,0,0, 0,0,64,0,0, 0,0,0,0,0, 0]

shiftFourBytes, shiftFiveBytes, shiftSixBytes, shiftEightBytes, shiftElevenBytes, shiftTwelveBytes :: forall repr . CoreISA repr => repr VR
shiftFourBytes   = uninteger 0x20
shiftFiveBytes   = uninteger 0x28
shiftSixBytes    = uninteger 0x30
shiftEightBytes  = uninteger 0x40
shiftElevenBytes = uninteger 0x58
shiftTwelveBytes = uninteger 0x60


-- DEBUG TOOLS

map4Tuple :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
map4Tuple f (t0, t1, t2, t3) = (f t0, f t1, f t2, f t3)

hexifyWords :: forall repr . CoreISA repr => (repr VR, repr VR, repr VR, repr VR) -> ([String], [String], [String], [String])
hexifyWords = map4Tuple (map (`Numeric.showHex` "") . wrds)




-- TEST CASES

case0 = let
      key0 = uninteger 0x00000000000000000000000000000000
      key1 = uninteger 0x00000000000000000000000000000000
      counter''nonce = unwrds [0x00000000, 0x00000000, 0x00000000, 0x00000000]
    in hexifyWords @Interp $ chacha20Block (key0, key1, counter''nonce)


-- CODEGRAPH
testChaChaBlock :: forall h. (Hardware h, CoreISA (Graph h)) =>
  Block h ((VR, VR, VR), (VR, VR, VR, VR))
testChaChaBlock = basicBlock "chachaBlock" ["v1","v2","v3"] ["v4","v5","v6","v7"] $ chacha20Block @(Graph h)

testChaChaCG :: CodeGraph CORE
testChaChaCG = createCG (genBlock testChaChaBlock)

-- TODO: What to do for the input register list?
testRegAllocateChaCha :: (RegMap CORE,SpillMap)
testRegAllocateChaCha =
  let schedule = defaultSchedule testChaChaCG
  in case fromRight (Map.empty,Map.empty,FGL.empty)
          $ regAllocateCG schedule testChaChaCG [("v1", "1"), ("v2", "2"), ("v3", "3")] of
          (rMap,sMap,_) -> (rMap,sMap)

testChaChaSchedGraph :: ScheduledGraph CORE
testChaChaSchedGraph = buildScheduledGraph (defaultSchedule testChaChaCG) testChaChaCG


testChaChaSim :: IO (Either (SimError CORE) (HardwareST CORE))
testChaChaSim =
    initSimulator
    "asm/testChaChaSim.dbxin"
    testChaChaSchedGraph
    testChaChaCG
    testRegAllocateChaCha
    initMRs
    ([], initVR)
  where
    key0 = uninteger 0x00000000000000000000000000000000
    key1 = uninteger 0x00000000000000000000000000000000
    counter''nonce = unwrds [0x00000000, 0x00000000, 0x00000000, 0x00000000]
    initVR = [("1", key0), ("2", key1), ("3", counter''nonce)]
    -- initGPR = [("3", unintegerG 4), ("4", unintegerG 1)]
    initMRs =
      []

-- testChaChaCodeGen :: ([ByteString], [ByteString])
-- testChaChaCodeGen =
--   let
--     sched = defaultSchedule testChaChaCG
--     (regMap,schedGraph) = case regAllocateCG sched testChaChaCG [("size","3"),("mrIn","2"),("mrOut","1")] of
--                               Right r -> r
--                               Left ns -> error $ "failed to register allocate with leftever nodes: "

--   in codeGenCG testChaChaCG (regMap,schedGraph) coreMetaData

-- printTestCodeGen :: IO ()
-- printTestCodeGen =
--   BS.putStrLn $ joinTuple $ bimap BS.unlines BS.unlines testChaChaCodeGen
--   where joinTuple (a, b) = a <> b

-- dotGenChaCha :: IO ()
-- dotGenChaCha = dotCompilation testChaChaCG

-- Test one quarter round
testQuarterRoundCG :: CodeGraph CORE
testQuarterRoundCG =
  let
    moduloB = genBlock $ moduloBlock
                         "chachaBlock"
                        ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
                        ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
                         $ quarterRoundsHeader @(Graph CORE)
  in
    createCG moduloB

testQuarterRoundCG' = fst $ fromJust $ moduloSchedule2S testQuarterRoundCG 1

tScale (n,dfNode) = 10000
testPenalties = [topBottomPenalty tScale,pushLoadConsumes]
testConstraints = [varBounds,completionConstraints,subjectConstraints,overwriteConstraints
                  ,latConstraints ,gprConstraints ]

testCodeGenQuarterRound :: IO ()
testCodeGenQuarterRound = do
  (cg,sched,regMap,spillMap,schedGraph,ii,stdgen') <- scheduleAndRegAllocate testQuarterRoundCG 1 coreMetaData testPenalties testConstraints Nothing False defSolver defStages [("size","3"),("mrIn","2"),("mrOut","1")]
  let (code,tables) = codeGenCG cg (regMap,spillMap,schedGraph) coreMetaData
      allCode =  code ++ tables
  BS.writeFile "asm/chacha20.s" $ BS.unlines allCode

testSimQuarterRound :: IO (Either (SimError CORE) (HardwareST CORE))
testSimQuarterRound = do
  (cg,sched,regMap,spillMap,schedGraph,ii,stdgen') <- scheduleAndRegAllocate testQuarterRoundCG 1 coreMetaData testPenalties testConstraints Nothing False defSolver defStages [("size","3"),("mrIn","2"),("mrOut","1")]
  -- dotScheduleCompilation cg schedGraph "."
  print regMap
  initSimulator
    "asm/testSimQuarterRound.dbxin"
    schedGraph
    cg
    (regMap,spillMap)
    initMRs
    (initGPRs, [])
  where
    key0, key1, counter''nonce :: forall repr. CoreISA repr => repr VR
    key0 = uninteger 0x4bf0ff27fb76d53b876fb664b7ed80ba
    key1 = uninteger 0x25393494b147e2649137455f9805fba
    counter''nonce = unwrds [0x1ff4652e, 0x2d472ddf, 0xa48c0c3c, 0xd191536f]
    constants = unwrds[0x61707865, 0x3320646e, 0x79622d32, 0x6b206574]
    initGPRs = [("3", unintegerG (2*8))]
    initMRs =
      [("mrIn", "2", [constants, key0, key1, counter''nonce])
      ,("mrOut", "1", map uninteger $ replicate 4 0)
      ]
{-
testRegAllocateQuarterRound :: RegMap CORE
testRegAllocateQuarterRound =
  let schedule = defaultSchedule testQuarterRoundCG
  in fromRight Map.empty $ regAllocateCG schedule testQuarterRoundCG []

testQuarterRoundGraph :: ScheduledGraph CORE
testQuarterRoundGraph = buildScheduledGraph (defaultSchedule testChaChaCG) testChaChaCG
-}
test_vector_sample = let

  key0, key1, counter''nonce :: forall repr. CoreISA repr => repr VR
  key0 = uninteger 0x4bf0ff27fb76d53b876fb664b7ed80ba
  key1 = uninteger 0x25393494b147e2649137455f9805fba
  counter''nonce = unwrds [0x1ff4652e, 0x2d472ddf, 0xa48c0c3c, 0xd191536f]
  constants = unwrds[0x61707865, 0x3320646e, 0x79622d32, 0x6b206574]

  in (hexifyWords @Interp $ quarterRounds (constants, key0 , key1, counter''nonce))

{-
(["19110d7c","f3e413f8","1bbf4f93","203ca7c0"],["8cdd8cd1","6b38491a","22462daa","9cb7bce8"],["c8b62ee9","f19aaadd","4ea9e7a6","2c17aeeb"],["b3bfe90b","921028e7","15ebcef4","7d555c95"])
-}
