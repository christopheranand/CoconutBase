{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AddChains where

import Coconut.BaseTypes
import Coconut.Core.CoreISA
import Coconut.Core.CodeGraph
import Coconut.Utils.CGWrappers
import Coconut.Graph.CodeGraph
import Coconut.Core.CoreHardware
import Coconut.Graph.Dot


-- 2^x - 4
-- TODO: Test with inlined
addChainsWrap :: CoreISA repr
  => (repr GPR,repr GPR,repr MR,repr MR,repr GPR,repr GPR)
  -> (repr GPR,repr GPR,repr MR,repr MR,repr GPR,repr GPR)
addChainsWrap (gCnt,gSize,mrIn,mrOut,fifo0,fifo1) =
  let
    (v0,mrIn0) = moduloVLoad mrIn 0 gCnt
    (v1,mrIn1) = moduloVLoad mrIn0 16 gCnt
    (v2,mrIn2) = moduloVLoad mrIn1 32 gCnt
    (v3,mrIn3) = moduloVLoad mrIn2 48 gCnt
    (v0',v1',v2',v3') = addChains (v0,v1,v2,v3)
    mrOut0 = moduloVStore mrOut 0 gCnt v0'
    mrOut1 = moduloVStore mrOut0 16 gCnt v1'
    mrOut2 = moduloVStore mrOut1 32 gCnt v2'
    mrOut3 = moduloVStore mrOut2 48 gCnt v3'
  in (gCnt,gSize,mrIn3,mrOut3,fifo0,fifo1)

addChains :: forall repr. CoreISA repr => (repr VR, repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
addChains =
      add2 . add1
    . add2 . add1
    . add2 . add1
    . add2 . add1
    . add2 . add1
    . add2 . add1
    . add2 . add1
    . add2 . add1
    . add2 . add1
    . add2 . add1
    . add2 . add1


add2 :: forall repr. CoreISA repr => (repr VR, repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
add2 (a, b, c, d) =
    let
        a' = va 2 a d
        d' = va 2 a' c
        d'' = va 2 b d'
    in (a', b, c, d'')

add1 :: forall repr. CoreISA repr => (repr VR, repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
add1 (a, b, c, d) =
    let
        b' = va 2 b c
        c' = va 2 b' d
        c'' = va 2 a c'
    in (a, b', c'', d)


addChainsCG :: CodeGraph CORE
addChainsCG =
  let
    moduloB = genBlock $ moduloBlock "moduloB"
                                    ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
                                    ["cnt","size","mrIn","mrOut","fifo0","fifo1"]
                                    $ addChainsWrap @(Graph CORE)
  in createCG moduloB
