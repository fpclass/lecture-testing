--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Fun with testing                                                  --
--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import Test.QuickCheck

import Picross

--------------------------------------------------------------------------------

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

-- | Run the tests when the program starts.
main = quickCheck prop_reverse

--------------------------------------------------------------------------------
