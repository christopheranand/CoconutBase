{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Coconut.BaseTypes
import Coconut.Core.CoreISA
import Coconut.Core.CodeGraph
import Coconut.Utils.CGWrappers
import Coconut.Graph.CodeGraph
import Coconut.Core.CoreHardware
import Coconut.Graph.Dot

-- addChains :: forall repr. CoreISA repr => (repr VR, repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
-- addChains = 
--       add2 . add1
--     . add2 . add1
--     . add2 . add1
--     . add2 . add1
--     . add2 . add1
--     . add2 . add1
--     . add2 . add1
--     . add2 . add1
--     . add2 . add1
--     . add2 . add1
--     . add2 . add1
    
-- add2 :: forall repr. CoreISA repr => (repr VR, repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
-- add2 (a, b, c, d) =
--     let
--         a' = va 2 a d
--         d' = va 2 a' c
--         d'' = va 2 b d'
--     in (a', b, c, d'')

-- add1 :: forall repr. CoreISA repr => (repr VR, repr VR, repr VR, repr VR) -> (repr VR, repr VR, repr VR, repr VR)
-- add1 (a, b, c, d) =
--     let
--         b' = va 2 b c
--         c' = va 2 b' d
--         c'' = va 2 a c'
--     in (a, b', c'', d)


-- cgTestAddChains :: forall h. Hardware h => CodeGraph h
-- cgTestAddChains =
--   createCG $ genBlock $ basicBlock "testAddChains"
--     ["v1", "v2", "v3", "v4"] ["v1", "v2", "v3", "v4"] (addChains @(Graph h))


main :: IO ()
main = putStrLn $ "Hello World"
  --unlines $ prettyCodeGraph @CORE cgTestAddChains

