module Minesweeper where

import Data.Maybe
import Data.Char
import Data.List
import System.Random
import Test.QuickCheck

data Minesweeper = Minesweeper {rows :: [[Maybe Int]]}
        deriving (Show, Eq)

allBlankMinesweeper :: Minesweeper
allBlankMinesweeper = Minesweeper (replicate 10 (replicate 10 (Just 0)))

bombMaker :: StdGen -> [Int] -> [Int]
bombMaker g list | length list == 10 = list 
                 | otherwise = bombMaker g' (nub (list ++ number))
                        where (number, g') = randomR (0, 99) g

--createMinesweeper