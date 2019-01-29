--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 11: Fun with testing                                               --
--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import Data.List (intersperse, intercalate)

import System.IO

import Picross

--------------------------------------------------------------------------------

-- | `renderRow` @cellWidth rowWidth row@ renders the @row where each cell
-- has a width represented by @cellWidth@ and the first column has a width of
-- @rowWidth@.
renderRow :: Int -> Int -> (String, [Int]) -> String
renderRow cw rw (l,r) = intercalate " | " (pad rw l : showRow cw r)

-- | `render` @solution state@ renders the current @state@ of the game for
-- an image represented by @solution@.
render :: [[Int]] -> [[Int]] -> String
render img sol = fr ++ "\n" ++ replicate (length fr) '-' ++ "\n" ++
             intercalate "\n" (map (renderRow cw rw) (zip rl sol))
    where rs = rows img
          cs = columns img
          rl = showLabels rs
          cl = showLabels cs
          rw = maximum [length l | l <- rl]
          cw = maximum [length l | l <- cl]
          fr = intercalate " | " (pad rw "" : [pad cw l | l <- cl])

-- | `loop` @solution state@ represents the main game loop. @solution@ is the
-- final image, while @state@ is the current image that has been solved so far.
loop :: [[Int]] -> [[Int]] -> IO ()
loop pic sol = do
    -- render the current state of the game
    putStrLn (render pic sol)

    -- prompt the user for coordinates at which they believe there to be a
    -- filled cell in the image.
    putStr "Column: "
    c <- readLn
    putStr "Row: "
    r <- readLn

    -- loop
    loop pic (guess (c,r) 1 pic sol)

-- | `main` is the main entry point to this application.
main :: IO ()
main = do
    -- disable output buffering
    hSetBuffering stdout NoBuffering
    -- start the main game loop
    loop test (makeBlank test)

--------------------------------------------------------------------------------
