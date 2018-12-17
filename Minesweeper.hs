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
                       [j 1,n  ,j 2,j 1,j 1, n,j 0,j 0,j 0,j 0 ],
                       [j 1,j 1,j 2,n  ,j 1,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 1,j 1,j 2,j 1,j 1,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 1,n  ,j 1,j 1,j 1,j 1 ],
                       [j 0,j 0,j 0,j 0,j 1,j 1,j 1,j 1,n  ,j 1 ]]
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

printMinesweeper :: Minesweeper -> IO ()
printMinesweeper minesweeper = putStrLn (unlines (map makeAString (rows minesweeper)))

makeAString :: [Maybe Int] -> [Char]
makeAString row = map makeChar row

-- turns the elements into characters
makeChar :: Maybe Int -> Char
makeChar Nothing = 'X'
makeChar (Just n) = intToDigit n

--Returns a Minesweeper with no bombs
allBlankMinesweeper :: Minesweeper
allBlankMinesweeper = Minesweeper (replicate 10 (replicate 10 (Just 0)))

type Pos = (Int,Int)

--Function to check how many neighbours are bombs and set number
checkNeighbours :: Minesweeper -> Pos -> Minesweeper
checkNeighbours minesw (x,y) 
    | x == 9 && y > 9 = minesw --checked all cells
    | y <= 9 && isBomb minesw (x,y) = checkNeighbours minesw (x,y+1) --cell is bomb, pass on to next cell in row
    | y > 9 = checkNeighbours minesw (x+1,0) --col index out of bounds, start on new row
    | y <= 9 = checkNeighbours (countSurroundingBombs minesw (x,y) (listNeighbours minesw (x,y))) (x,y+1)--count number of bombs surrounding cell
    | otherwise = error "checkNeighbours not working"

--Count surrounding bombs and set cell to that number.
countSurroundingBombs :: Minesweeper -> Pos -> [Maybe Int] -> Minesweeper
countSurroundingBombs minesw (x,y) list = Minesweeper { rows = (rows minesw !!= (x, ((rows minesw !! x) !!= (y, Just count)))) }
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
isBomb :: Minesweeper -> Pos -> Bool
isBomb mine (x,y) = isNothing ((rows mine !! x) !! y)
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


{-runGame = do putStrLn "Welcome to the Game"
             g <- newStdGen
             bombMaker g list-}

--createMinesweeper
