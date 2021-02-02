--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Fun with testing                                                  --
--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import Test.HUnit

import Picross

--------------------------------------------------------------------------------

test1 :: Test
test1 = TestCase (assertEqual "for (symbol 0)," ' ' (symbol 0))

test2 :: Test 
test2 = TestCase (assertEqual "for (symbol 1)," 'X' (symbol 1))

tests :: Test
tests = TestList [ TestLabel "symbol test" test1
                 , TestLabel "symbol test (2)" test2]

-- | Run the tests when the program starts.
main = runTestTT tests

--------------------------------------------------------------------------------
