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
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 1,j 1,j 1,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 1,n  ,j 2,j 1,j 1, n,j 0,j 0,j 0,j 0 ],
                       [j 1,j 1,j 2,n  ,j 1,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 1,j 1,j 2,j 1,j 1,j 0,j 0,j 0 ],
                       [j 1,j 1,j 0,j 0,j 1,n  ,j 1,j 1,j 1,j 1 ],
                       [n  ,j 1,j 0,j 0,j 1,j 1,j 1,j 1,n  ,j 1 ]]
 where
   n = Nothing
   j = Just

example2 :: Minesweeper
example2 = Minesweeper [[n , n ,j 0,j 0,j 0, n,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,n  ,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,n  ,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,n  ,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,n  ,j 0 ]]
    where
      n = Nothing
      j = Just

printActualMinesweeper :: Minesweeper -> [Char] -> IO ()
printActualMinesweeper minesweeper string= putStrLn (unlines (map makeActualString (rows minesweeper)) ++ string)

makeActualString :: [Maybe Int] -> [Char]
makeActualString row = concatMap makeActualChar row

makeActualChar :: Maybe Int -> [Char]
makeActualChar Nothing = "* "
makeActualChar (Just n) | n == 0    = "_ "
                        | n == 9   = "* "
                        | otherwise = [(intToDigit n)] ++ " "

printMinesweeper :: Minesweeper -> [Char] ->  IO ()
printMinesweeper minesweeper string = putStrLn (unlines (map makeAString (rows minesweeper)) ++ string)

makeAString :: [Maybe Int] -> [Char]
makeAString row = concatMap makeChar row

-- turns the elements into characters
makeChar :: Maybe Int -> [Char]
makeChar Nothing              = "+ "
makeChar (Just n) | n == 0   = "+ "
                  | n == 9   = "X "
                  | mod n 10 == 0 = "_ "
                  | n > 10    = [intToDigit (mod n 10)] ++ " "
                  | otherwise = "+ "

openCell :: Minesweeper -> Pos -> Minesweeper
openCell minesweeper (x,y) = Minesweeper {rows = (!!=) (rows minesweeper) (abs x, row')}
  where
    row  = (!!) (rows minesweeper) (abs x)
    row' = (!!=) row (abs y, checkContent minesweeper (x,y))

checkContent :: Minesweeper -> Pos -> Maybe Int
checkContent minesweeper (x,y) | isNothing ((rows minesweeper !! x) !! y) = Just 9
                               | pos == 0 = Just 10
                               | otherwise = Just (pos + 10)
  where Just pos = (rows minesweeper !! x) !! y

--openCells :: Minesweeper ->  Pos -> Maybe Int -> Maybe Int -> Minesweeper
--openCells mine pos@(x,y) target replacement =
 --   if((not $ isValid pos) || getValue mine (x,y) /= target) then mine 
  --  else 
   --   north
    --  where mine' = changeCell mine pos replacement
     --       east = openCells mine' (x+1, y) target replacement
      --      west = openCells east (x-1, y) target replacement
       --     south = openCells west (x, y+1) target replacement
        --    north = openCells south (x, y-1) target replacement
--if Nothing then do nothing
--if Just n, where n > 0 then open then nothing
--else continue

openCells :: Minesweeper ->  Pos -> Minesweeper
openCells mine pos@(x,y)
    | ((not $ isValid pos) || getValue mine pos == Nothing) = mine 
    | getValue mine pos /= (Just 0) = openCell' mine pos 
    | otherwise = north
          where mine' = changeCell mine pos (Just 10)
                east = openCells mine' (x+1, y) 
                west = openCells east (x-1, y) 
                south = openCells west (x, y+1) 
                north = openCells south (x, y-1) 

openCell' :: Minesweeper -> Pos -> Minesweeper
openCell' mine (x,y) = changeCell' mine (x,y) (Just 10)

--Change a value at a given position to a given value
changeCell :: Minesweeper -> Pos -> Maybe Int -> Minesweeper
changeCell mine (x,y) value = if isValid (x,y) then Minesweeper { rows = (rows mine !!= (x, ((rows mine !! x) !!= (y, value)))) } else mine

changeCell' :: Minesweeper -> Pos -> Maybe Int -> Minesweeper
changeCell' mine (x,y) (Just n) = if isValid (x,y) then Minesweeper { rows = (rows mine !!= (x, ((rows mine !! x) !!= (y, Just (n+old))))) } else mine
    where Just old = getValue mine (x,y)

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
              | x == Just 9 = True
              | otherwise    = isBomb xs

finishGame :: Minesweeper -> IO ()
finishGame minesweeper = printActualMinesweeper minesweeper "Game Over!"

--Returns a Minesweeper with no bombs
allBlankMinesweeper :: Minesweeper
allBlankMinesweeper = Minesweeper (replicate 10 (replicate 10 (Just 0)))

--Function to check how many neighbours are bombs and set number
checkNeighbours :: Minesweeper -> Pos -> Minesweeper
checkNeighbours minesw (x,y) 
    | x == 9 && y > 9 = minesw --checked all cells
    | y <= 9 && isBomb' minesw (x,y) = checkNeighbours minesw (x,y+1) --cell is bomb, pass on to next cell in row
    | y > 9 = checkNeighbours minesw (x+1,0) --col index out of bounds, start on new row
    | y <= 9 = checkNeighbours (countSurroundingBombs minesw (x,y) (listNeighbours minesw (x,y))) (x,y+1)--count number of bombs surrounding cell
    | otherwise = error "checkNeighbours not working"

--Count surrounding bombs and set cell to that number.
countSurroundingBombs :: Minesweeper -> Pos -> [Maybe Int] -> Minesweeper
countSurroundingBombs minesw (x,y) list = changeCell minesw (x,y) (Just count)
        where count = length (filter isNothing list)

-- | An operator that changes a value at a specific index to a new one
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) []      _             = error "Empty list"
(!!=) (x:xs) (index, value) | length (x:xs) < index = error "Index out of bounds"
(!!=) (x:xs) (0, value)     = value : xs
(!!=) (x:xs) (index, value) = x : xs !!= (index-1, value)

--Make  list of neighbours
listNeighbours :: Minesweeper -> Pos -> [Maybe Int]
listNeighbours mine (x,y) = [getValue mine ((x-1),(y-1))]  ++ [getValue mine ((x-1),y)] ++ [getValue mine ((x-1),(y+1))]
                       ++ [getValue mine (x,(y-1))] ++ [getValue mine (x,(y+1))]
                       ++ [getValue mine ((x+1),(y-1))] ++ [getValue mine ((x+1),y)] ++ [getValue mine ((x+1),(y+1))]

--Get value at a position in Minesweeper
getValue :: Minesweeper -> Pos -> Maybe Int
getValue mine (x,y)
        | isValid (x,y) = (rows mine !! x) !! y
        | otherwise = Just 0
                        
--Checks if a position is valid
isValid :: Pos -> Bool
isValid (x,y) = x' && y'
    where x' = x >= 0 && x <= 9 
          y' = y >= 0 && y <= 9 

--Helper function to checkneighbours, checks if a cell is a bomb
isBomb' :: Minesweeper -> Pos -> Bool
isBomb' mine (x,y) = isNothing ((rows mine !! x) !! y)
--start at 0,0
--recirsively pass on to down and right neighbour
--check if pos is a bomb
    --if bomb: pass on
    --else check all neighbours is bomb
    --start counter and set number to counter, pass on

--bombMaker :: StdGen -> [Int] -> [Int]
--bombMaker g list | length list == 10 = list
 --                | otherwise = bombMaker g' newList
  --                  where (number, g') = randomR (0, 99) g
   --                       newList = nub (list ++ [number])

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
         else gameLoop (openCells minesweeper (findFirst cell))
       else do putStrLn ("Please choose a correct position")
               gameLoop minesweeper
