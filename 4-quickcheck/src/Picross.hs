--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Fun with testing                                                  --
--------------------------------------------------------------------------------

module Picross where

import Data.List (intersperse, transpose, group, intercalate)

--------------------------------------------------------------------------------

-- | A test image, we represent filled cells as 1 and empty cells as 0. We
-- will later represent blank cells (ones that have not been guessed yet) as -1.
test :: [[Int]]
test = [ [ 0, 0, 1, 0, 0 ]
       , [ 0, 1, 1, 1, 0 ]
       , [ 1, 1, 0, 1, 1 ]
       , [ 1, 0, 1, 0, 1 ]
       , [ 0, 1, 0, 1, 0 ]
       ]

-- | Given a row or column @xs@, `sequences` @xs@ calculates the lengths of
-- uninterrupted sequences of filled cells.
sequences :: [Int] -> [Int]
sequences xs = [length ys | ys <- group xs, head ys /= 0]

-- | `rows` @rows@ calculates the sequence lengths for the @rows@ of an image.
rows :: [[Int]] -> [[Int]]
rows = map sequences

-- | `columns` @columns@ calculates the sequence lengths for the @columns@
-- of an image.
columns :: [[Int]] -> [[Int]]
columns = map sequences . transpose

-- | `makeBlank` @img@ replaces all cells in @img@ with blank ones (i.e. ones
-- that have not been guessed yet)
makeBlank :: [[Int]] -> [[Int]]
makeBlank = map (map (const (-1)))

-- | `symbol` @cell@ maps @cell@ to a corresponding character symbol.
symbol :: Int -> Char
symbol 0    = ' '
symbol 1    = 'X'
symbol (-1) = '?'

-- | `showCell` @width cell@ renders @cell@ as a sequence of @width@ characters.
showCell :: Int -> Int -> String
showCell w c = replicate w (symbol c)

-- | `showRow` @width row@ renders @row@ so that every cell has a width of
-- @width@.
showRow :: Int -> [Int] -> [String]
showRow w = map (showCell w)

-- | `pad` @n str@ pads @str@ to a length of @n@ with spaces at the end.
pad :: Int -> String -> String
pad n xs = xs ++ replicate (n - length xs) ' '

-- | `showLabel` @label@ renders @label@ as a string.
showLabel :: [Int] -> String
showLabel = intercalate ","  . map show

-- | `showLabels` @labels@ renders @labels@ as strings.
showLabels :: [[Int]] -> [String]
showLabels = map showLabel

-- | `testCell` @pos type image@ tests whether @image@ contains a cell whose
-- value is @type@ in the position represented by @pos@.
testCell :: (Int, Int) -> Int -> [[Int]] -> Bool
testCell (c,r) w img = w == (img !! r) !! c

-- | `updateRow` @index value row@ replaces the element at @index@ with @value@
-- in @row@.
updateRow :: Int -> a -> [a] -> [a]
updateRow n c xs = take n xs ++ [c] ++ drop (n+1) xs

-- | `guess` @pos type state solution@ makes a guess that the cell at the
-- coordinates represented by @pos@ has a value of @w@ in the @solution@. If the
-- guess is correct, the current @state@ of the game is updated with @w@ in the
-- position represented by @pos@.
guess :: (Int, Int) -> Int -> [[Int]] -> [[Int]] -> [[Int]]
guess (c,r) w img st
    | testCell (c,r) w img = let (xs, ys) = splitAt r st
                             in xs ++ [updateRow c w (head ys)] ++ tail ys
    | otherwise            = st

--------------------------------------------------------------------------------
