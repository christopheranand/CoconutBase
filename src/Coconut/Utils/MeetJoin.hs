-- |
-- Module      :  Coconut.Utils.MeetJoin
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- Meet instructions assert that a set of nodes are equivalent.
-- For memory regions, this means that the regions have a common ancestor,
-- and the set of loads and stores are independent of each other.
-- Currently, we take this to mean that there are no accesses to the same address
-- form different functional threads of execution,
-- although there could be multiple accesses within a single thread of execution,
-- and that there have been no intermediate meets.
-- This will cause many valid programs to be flagged as unsafe,
-- including programs which declare intermediate merges,
-- and programs containing unordered stores which necessarily store the same value.
-- In the first case, memory verification would consume more memory,
-- and in the second case, we would need to model all assembly instructions.
-- The resulting graph edge has homogenous tentacles,
-- and does not normally generate code,
-- although we could optionally generate assertions.

module Coconut.Utils.MeetJoin where

-- | Coconut generates memory regions every time loads/stores occur (each seperate nodes in the CodeGraph),
-- a meet is a special edge that combines those memory regions into one
-- so instructions can safely acces them
class HasMeet eq node where
  meet :: String   -- name of meet point, useful for
     -> eq         -- equivalence property (to be used in future verification)
     -> [node]     -- nodes which contribute to result
     -> node

{-

For register values, we take this to mean that the scheduler is free to use either code sequence.
Could be instantiated for algebraic data types.

|join reason eq xs| expresses that all elements of |xs|
are equivalent with respect to the equivalence relation |eq|.
-}
class HasJoin eq node where
  join :: String   -- name/reason for join point, useful for debugging
     -> eq         -- equivalence relation (to be used in future verification)
     -> [node]     -- nodes which are equivalent
     -> node
