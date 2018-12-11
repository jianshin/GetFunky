module Minesweeper where

import Data.Maybe
import Data.Char
import Data.List
import System.Random
import Test.QuickCheck

data Minesweeper = Minesweeper {rows :: [[Maybe Int]]}
        deriving (Show, Eq)

type Pos = (Int, Int)


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
makeChar Nothing              = '.'
makeChar (Just n) | n == 10   = ' '
                  | n == 20   = 'X'
                  | n > 10    = intToDigit (n - 10)
                  | otherwise = '.'

openCell :: Minesweeper -> Pos -> Minesweeper
openCell minesweeper (x,y) = Minesweeper {rows = (!!=) (rows minesweeper) (abs x, row')}
  where
    row  = (!!) (rows minesweeper) (abs x)
    row' = (!!=) row (abs y, checkContent minesweeper (x,y))

checkContent :: Minesweeper -> Pos -> Maybe Int
checkContent minesweeper (x,y) | isNothing ((rows minesweeper !! x) !! y) = Just 20
                               | pos == 0 = Just 10
                               | otherwise = Just (pos + 10)
  where Just pos = ((rows minesweeper !! x) !! y)

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) []      _             = error "Empty list"
(!!=) (x:xs) (index, value) | length (x:xs) < index = error "Index out of bounds"
(!!=) (x:xs) (0, value)     = value : xs
(!!=) (x:xs) (index, value) = x : xs !!= (index-1, value)


gameOver :: Minesweeper -> Bool
gameOver minesweeper = getRows (rows minesweeper)

getRows :: [[Maybe Int]] -> Bool
getRows []                          = False
getRows (x:xs) | (isBomb x) == True = True
               | otherwise          = getRows xs

isBomb :: [Maybe Int] -> Bool
isBomb []                    = False
isBomb (x:xs) | x == Nothing = isBomb xs
              | x == Just 20 = True
              | otherwise    = isBomb xs



allBlankMinesweeper :: Minesweeper
allBlankMinesweeper = Minesweeper (replicate 10 (replicate 10 (Just 0)))












{-bombMaker :: StdGen -> [Int] -> [Int]
bombMaker g list | length list == 10 = list
                 | otherwise = bombMaker g' newList
                    where (number, g') = randomR (0, 99) g
                          newList = nub (list ++ [number])

implementation = Interface
  { iOpenCell         = openCell
  , iPrintMinesweeper = printMinesweeper
  , iBombmaker        = bombMaker
  }

main :: IO ()
main = runGame implementation

runGame :: Interface -> IO ()
rungame i =
  do putStrLn "Welcome to Minesweeper Pro"
     g <- newStdGen
     gameLoop i

gameLoop-}
