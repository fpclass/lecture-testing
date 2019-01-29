--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 11: Fun with testing                                               --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

--------------------------------------------------------------------------------

import Test.Hspec
import Test.Hspec.QuickCheck

import Picross

--------------------------------------------------------------------------------

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

-- | Run the tests when the program starts.
main = hspec $ do
    describe "symbol" $ do
        it "symbol 0 == ' '" (symbol 0 == ' ')
        it "symbol 1 == 'X'" (symbol 1 == 'X')
        it "symbol (-1) == '?'" (symbol (-1) == '?')
    describe "reverse" $ do
        prop "reverse (reverse xs) == xs"
            (\(xs :: [Int]) -> reverse (reverse xs) == xs)

--------------------------------------------------------------------------------
