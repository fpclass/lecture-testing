--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 11: Fun with testing                                               --
--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import Test.HUnit

import Picross

--------------------------------------------------------------------------------

test1 :: Test
test1 = TestCase (assertEqual "for (symbol 0)," ' ' (symbol 0))

tests :: Test
tests = TestList [TestLabel "symbol test" test1]

-- | Run the tests when the program starts.
main = runTestTT tests

--------------------------------------------------------------------------------
