module Minesweeper where

import Data.Maybe
import Data.Char
import Data.List
import System.Random
import Test.QuickCheck

data Minesweeper = Minesweeper {rows :: [[Maybe Int]]}
        deriving (Show, Eq)


example :: Minesweeper
example = Minesweeper [[n  ,n  ,j 1,j 0,j 1,n  ,j 1,j 0,j 0,j 0 ],
                       [j 2,j 2,j 1,j 0,j 1,j 1,j 1,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 1,j 1,j 1,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 1,n  ,j 2,j 1,j 1,j 0,j 0,j 0,j 0,j 0 ],
                       [j 1,j 1,j 2,n  ,j 1,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 1,j 1,j 2,j 1,j 1,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 1,n  ,j 1,j 1,j 1,j 1 ],
                       [j 0,j 0,j 0,j 0,j 1,j 1,j 1,j 1,n  ,j 1 ]]
 where
   n = Nothing
   j = Just

printMinesweeper :: Minesweeper -> IO ()
printMinesweeper minesweeper = putStrLn (unlines (map makeAString (rows minesweeper)))

makeAString :: [Maybe Int] -> [Char]
makeAString row = map makeChar row

-- turns the elements into characters
makeChar :: Maybe Int -> Char
makeChar Nothing = 'X'
makeChar (Just n) = intToDigit n


allBlankMinesweeper :: Minesweeper
allBlankMinesweeper = Minesweeper (replicate 10 (replicate 10 (Just 0)))












bombMaker :: StdGen -> [Int] -> [Int]
bombMaker g list | length list == 10 = list
                 | otherwise = bombMaker g' newList
                    where (number, g') = randomR (0, 99) g
                          newList = nub (list ++ [number])


{-runGame = do putStrLn "Welcome to the Game"
             g <- newStdGen
             bombMaker g list-}

--createMinesweeper
