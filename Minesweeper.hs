module Minesweeper where

import Data.Maybe
import Data.Char
import Data.List
import Test.QuickCheck

data Minesweeper = Minesweeper {rows :: [[Maybe Int]]}
        deriving (Show, Eq)

allBlankMinesweeper :: Minesweeper
allBlankMinesweeper = Minesweeper (replicate 10 (replicate 10 (Just 0)))

--creates sudoku cells
cell :: Gen (Maybe Int)
cell = frequency [(1, return Nothing), (9, return (Just 0))]

--C2
-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Minesweeper where
    arbitrary =
      do rows <- vectorOf 10 (vectorOf 10 cell)
         return (Minesweeper rows)