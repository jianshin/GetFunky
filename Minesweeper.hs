module Minesweeper where

import Data.Maybe
import Data.Char
import Data.List
import System.Random hiding (shuffle)
import Test.QuickCheck

data Minesweeper = Minesweeper {rows :: [[Maybe Int]]}
        deriving (Show, Eq)

type Pos = (Int, Int)


example :: Minesweeper
example = Minesweeper [[n  ,n  ,j 1,j 0,j 1,n  ,j 1,j 0,j 0,j 0 ],
                       [j 2,j 2,j 1,j 0,j 1,j 1,j 1,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 1,j 1,j 1,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 1,n  ,j 1,j 0,j 0,j 0 ],
                       [j 1,j 1,j 1,j 0,j 1,j 1,j 2,j 1,j 0,j 0 ],
                       [j 1,n  ,j 2,j 1,j 1,j 1,n  ,j 1,j 0,j 0 ],
                       [j 1,j 1,j 2,n  ,j 1,j 1,j 1,j 1,j 0,j 0 ],
                       [j 0,j 0,j 1,j 1,j 2,j 1,j 1,j 0,j 0,j 0 ],
                       [j 1,j 1,j 0,j 0,j 1,n  ,j 1,j 1,j 1,j 1 ],
                       [n  ,j 1,j 0,j 0,j 1,j 1,j 1,j 1,n  ,j 1 ]]
 where
   n = Nothing
   j = Just

printActualMinesweeper :: Minesweeper -> [Char] -> IO ()
printActualMinesweeper minesweeper string= putStrLn (unlines (map makeActualString (rows minesweeper)) ++ string)

makeActualString :: [Maybe Int] -> [Char]
makeActualString row = map makeActualChar row

makeActualChar :: Maybe Int -> Char
makeActualChar Nothing = '*'
makeActualChar (Just n) | n == 0    = '_'
                        | n == 20   = '*'
                        | otherwise = (intToDigit n)

printMinesweeper :: Minesweeper -> [Char] ->  IO ()
printMinesweeper minesweeper string = putStrLn (unlines (map makeAString (rows minesweeper)) ++ string)

makeAString :: [Maybe Int] -> [Char]
makeAString row = map makeChar row

-- turns the elements into characters
makeChar :: Maybe Int -> Char
makeChar Nothing              = '+'
makeChar (Just n) | n == 10   = '_'
                  | n == 20   = 'X'
                  | n > 10    = intToDigit (n - 10)
                  | otherwise = '+'

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

wonGame :: Minesweeper -> Bool
wonGame minesweeper = getWonGameRows (rows minesweeper)

getWonGameRows :: [[Maybe Int]] -> Bool
getWonGameRows []                                = True
getWonGameRows (x:xs) | (isOkContent x) == False = False
                      | otherwise                = getWonGameRows xs

isOkContent :: [Maybe Int] -> Bool
isOkContent [] = True
isOkContent (x:xs) | x > Just 10 = True
                   | x == Just 0 = True
                   | otherwise = False

gameOver :: Minesweeper -> Bool
gameOver minesweeper = getGameOverRows (rows minesweeper)

getGameOverRows :: [[Maybe Int]] -> Bool
getGameOverRows []                          = False
getGameOverRows (x:xs) | (isBomb x) == True = True
                       | otherwise          = getGameOverRows xs

isBomb :: [Maybe Int] -> Bool
isBomb []                    = False
isBomb (x:xs) | x == Nothing = isBomb xs
              | x == Just 20 = True
              | otherwise    = isBomb xs

finishGame :: Minesweeper -> IO ()
finishGame minesweeper = printActualMinesweeper minesweeper "Game Over!"



allBlankMinesweeper :: Minesweeper
allBlankMinesweeper = Minesweeper (replicate 10 (replicate 10 (Just 0)))




findFirst :: [Char] -> Pos
findFirst [] = (11,11)
findFirst (x:xs) | isDigit x = findSecond xs (digitToInt x)
                 | otherwise = findFirst xs

findSecond :: [Char] -> Int -> Pos
findSecond [] _ = (11,11)
findSecond (x:xs) int | isDigit x = (int, (digitToInt x))
                      | otherwise = findSecond xs int


bombMaker :: StdGen -> [Int] -> [Int]
bombMaker g list | length list == 10 = list
                 | otherwise = bombMaker g' newList
                    where (number, g') = randomR (0, 99) g
                          newList = nub (list ++ [number])

main :: IO ()
main = do putStrLn "Welcome to Minesweeper Pro"
          g <- newStdGen
          gameLoop example

a = [1..19]

--printBombMaker :: [Int] -> IO ()
--printBombMaker list = putStrLn (unlines testChar list)

--test :: IO ()
--test = do putStrLn "Test"
--          g <- newStdGen
--          printBombMaker (bombMaker g a)


testChar :: [Int] -> [Char]
testChar [] = []
testChar (x:xs) = [(intToDigit x)] ++ testChar xs


gameLoop :: Minesweeper -> IO ()
gameLoop minesweeper =
  do printMinesweeper minesweeper "Open a block with (x,y)"
     cell <- getLine
     if not (findFirst cell == (11,11))
       then if gameOver $ openCell minesweeper (findFirst cell)
         then finishGame $ openCell minesweeper (findFirst cell)
         else gameLoop (openCell minesweeper (findFirst cell))
       else do putStrLn ("Please choose a correct position")
               gameLoop minesweeper
