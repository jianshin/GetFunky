module Minesweeper where

import Data.Maybe
import Data.Char
import Data.List
import System.Random hiding (shuffle)
import Test.QuickCheck hiding (shuffle)

data Minesweeper = Minesweeper {rows :: [[Maybe Int]]}
        deriving (Show, Eq)

type Pos = (Int, Int)

cell :: Gen (Maybe Int)
cell = frequency [(8, do n <- choose (0,8)
                         return (Just n)), (2, return Nothing)]

instance Arbitrary Minesweeper where
  arbitrary =
    do rows <- vectorOf 10 (vectorOf 10 cell)
       return (Minesweeper rows)


example :: Minesweeper
example = Minesweeper [[n  ,j 0,j 1,j 0,j 1,n  ,j 1,j 0,j 1,j 0 ],
                       [j 2,j 2,j 1,j 0,j 1,j 1,j 1,j 0,j 1,j 1 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 1,j 1,j 1,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 1,j 0,j 2,j 1,j 1,j 0,j 0,j 0,j 0,j 0 ],
                       [j 1,j 1,j 2,j 0,j 1,j 0,j 0,j 0,j 0,j 0 ],
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

example3 :: Minesweeper
example3 = Minesweeper [[j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0 ],
                       [j 0,j 0,j 0,j 0,j 0,j 0,j 0,j 0,n  ,j 0 ]]
    where n = Nothing
          j = Just

-- | A function that checks that the input minesweeper is a valid minesweeper
isMinesweeper :: Minesweeper -> Bool
isMinesweeper minesweeper = length (rows minesweeper) == 10 && and
  (map (\row -> length row == 10) (rows minesweeper))

-- | A function that prints the minesweeper under the hood in case you loose
printActualMinesweeper :: Minesweeper -> [Char] -> IO ()
printActualMinesweeper minesweeper string= putStrLn (unlines (map makeActualString (rows minesweeper)) ++ string)

-- | A helperfunction for printing the minesweeper in case you loose
makeActualString :: [Maybe Int] -> [Char]
makeActualString row = concatMap makeActualChar row

-- | A helperfunction for printing the minesweeper in case you loose
makeActualChar :: Maybe Int -> [Char]
makeActualChar Nothing = "* "
makeActualChar (Just n) | n == 0          = "_ "
                        | n == 9          = "* "
                        | (mod n 10) == 0 = "_ "
                        | otherwise       = [intToDigit (mod n 10)] ++ " "

-- | A function that prints the minesweeper
printMinesweeper :: Minesweeper -> [Char] ->  IO ()
printMinesweeper minesweeper string = putStrLn (unlines (map makeAString (rows minesweeper)) ++ string)

-- | A helperfunction for printing the minesweeper
makeAString :: [Maybe Int] -> [Char]
makeAString row = concatMap makeChar row

-- | A helperfunction for printing the minesweeper by turning the elements into characters
makeChar :: Maybe Int -> [Char]
makeChar Nothing                  = "+ "
makeChar (Just n) | n == 0        = "+ "
                  | n == 9        = "X "
                  | n > 99        = "? "
                  | mod n 10 == 0 = "_ "
                  | n == 19       = "? "
                  | n > 10        = [intToDigit (mod n 10)] ++ " "
                  | otherwise     = "+ "

-- | A function that opens a specified cell
openCell :: Minesweeper -> Pos -> Minesweeper
openCell minesweeper (x,y) = Minesweeper {rows = (!!=) (rows minesweeper) (abs x, row')}
  where
    row  = (!!) (rows minesweeper) (abs x)
    row' = (!!=) row (abs y, checkContent minesweeper (x,y))

-- | A function that changes the value of the specified position so that the opened cell shows
checkContent :: Minesweeper -> Pos -> Maybe Int
checkContent minesweeper (x,y) | isNothing ((rows minesweeper !! x) !! y) = Just 9
                               | pos == 0 = Just 10
                               | otherwise = Just (pos + 10)
  where Just pos = (rows minesweeper !! x) !! y

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

-- | Change a value at a given position to a given value
changeCell :: Minesweeper -> Pos -> Maybe Int -> Minesweeper
changeCell mine (x,y) value = if isValid (x,y) then Minesweeper { rows = (rows mine !!= (x, ((rows mine !! x) !!= (y, value)))) } else mine

changeCell' :: Minesweeper -> Pos -> Maybe Int -> Minesweeper
changeCell' mine (x,y) (Just n) = if isValid (x,y) then Minesweeper { rows = (rows mine !!= (x, ((rows mine !! x) !!= (y, Just (n+old))))) } else mine
    where Just old = getValue mine (x,y)

-- | A function that checks if the game is won
wonGame :: Minesweeper -> Bool
wonGame minesweeper = getWonGameRows (rows minesweeper)

-- | A helperfunction for checking if all rows meet the criteria for winning
getWonGameRows :: [[Maybe Int]] -> Bool
getWonGameRows []                                = True
getWonGameRows (x:xs) | (isOkContent x) == False = False
                      | otherwise                = getWonGameRows xs

-- | A helperfunction for checking if all elements in the rows are opened or are bombs
isOkContent :: [Maybe Int] -> Bool
isOkContent [] = True
isOkContent (x:xs) | x == Nothing         = isOkContent xs
                   | (n > 9) && (n < 100) = isOkContent xs
                   | otherwise            = False
  where (Just n) = x

-- | A function that checks if the game is lost
gameOver :: Minesweeper -> Bool
gameOver minesweeper = getGameOverRows (rows minesweeper)

-- | A helperfunction for checking if a row contains an opened bomb
getGameOverRows :: [[Maybe Int]] -> Bool
getGameOverRows []                          = False
getGameOverRows (x:xs) | (isBomb x) == True = True
                       | otherwise          = getGameOverRows xs

-- | A helperfunction that checks all if an element in a list is a opened bomb
isBomb :: [Maybe Int] -> Bool
isBomb []                    = False
isBomb (x:xs) | x == Nothing = isBomb xs
              | x == Just 9 = True
              | otherwise    = isBomb xs

-- | A function that in case of a loss prints how the minesweeper looks under the hood
finishGame :: Minesweeper -> IO ()
finishGame minesweeper = printActualMinesweeper minesweeper "Game Over!"

-- | Returns a Minesweeper with no bombs
allBlankMinesweeper :: Minesweeper
allBlankMinesweeper = Minesweeper (replicate 10 (replicate 10 (Just 0)))

-- | Function to check how many neighbours are bombs and set number
checkNeighbours :: Minesweeper -> Pos -> Minesweeper
checkNeighbours minesw (x,y)
    | x == 9 && y > 9 = minesw --checked all cells
    | y <= 9 && isBomb' minesw (x,y) = checkNeighbours minesw (x,y+1) --cell is bomb, pass on to next cell in row
    | y > 9 = checkNeighbours minesw (x+1,0) --col index out of bounds, start on new row
    | y <= 9 = checkNeighbours (countSurroundingBombs minesw (x,y) (listNeighbours minesw (x,y))) (x,y+1)--count number of bombs surrounding cell
    | otherwise = error "checkNeighbours not working"

-- | Count surrounding bombs and set cell to that number.
countSurroundingBombs :: Minesweeper -> Pos -> [Maybe Int] -> Minesweeper
countSurroundingBombs minesw (x,y) list = changeCell minesw (x,y) (Just count)
        where count = length (filter isNothing list)

-- | An operator that changes a value at a specific index to a new one
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) []      _             = error "Empty list"
(!!=) (x:xs) (index, value) | length (x:xs) < index = error "Index out of bounds"
(!!=) (x:xs) (0, value)     = value : xs
(!!=) (x:xs) (index, value) = x : xs !!= (index-1, value)

-- | Make  list of neighbours
listNeighbours :: Minesweeper -> Pos -> [Maybe Int]
listNeighbours mine (x,y) = [getValue mine ((x-1),(y-1))]  ++ [getValue mine ((x-1),y)] ++ [getValue mine ((x-1),(y+1))]
                       ++ [getValue mine (x,(y-1))] ++ [getValue mine (x,(y+1))]
                       ++ [getValue mine ((x+1),(y-1))] ++ [getValue mine ((x+1),y)] ++ [getValue mine ((x+1),(y+1))]

-- | Get value at a position in Minesweeper
getValue :: Minesweeper -> Pos -> Maybe Int
getValue mine (x,y)
        | isValid (x,y) = (rows mine !! x) !! y
        | otherwise = Just 0

-- | Checks if a position is valid
isValid :: Pos -> Bool
isValid (x,y) = x' && y'
    where x' = x >= 0 && x <= 9
          y' = y >= 0 && y <= 9

-- | Helper function to checkneighbours, checks if a cell is a bomb
isBomb' :: Minesweeper -> Pos -> Bool
isBomb' mine (x,y) = isNothing ((rows mine !! x) !! y)

-- | A function that finds the first number that is used in the position, in case the first char is 'F' or 'O' then putFlag or openFlag is called
findFirst :: [Char] -> Pos
findFirst [] = (11,11)
findFirst (x:xs) | isDigit x = findSecond xs (digitToInt x)
                 | x == 'F' = checkIfFlagInputIsCorrect xs
                 | x == 'O' = checkIfOpenInputIsCorrect xs
                 | otherwise = (11,11)

-- | Checks to see if the second character is a digit, if so it will return the position that should be opened
findSecond :: [Char] -> Int -> Pos
findSecond [] _ = (11,11)
findSecond (x:xs) int | isDigit x = (int, (digitToInt x))
                      | otherwise = (11,11)

-- | Checks that the length of the input is 2 characters long
checkIfFlagInputIsCorrect :: [Char] -> Pos
checkIfFlagInputIsCorrect []                        = (11,11)
checkIfFlagInputIsCorrect list | (length list) == 2 = checkIfFlagInputIsCorrect' list
                               | otherwise          = (11,11)

-- | Checks that the input only consists of 2 digits
checkIfFlagInputIsCorrect' :: [Char] -> Pos
checkIfFlagInputIsCorrect' []                 = (12,12)
checkIfFlagInputIsCorrect' (x:xs) | isDigit x = checkIfFlagInputIsCorrect' xs
                                  | otherwise = (11,11)

-- | Checks that the length of the input is 2 characters long
checkIfOpenInputIsCorrect :: [Char] -> Pos
checkIfOpenInputIsCorrect []                        = (11,11)
checkIfOpenInputIsCorrect list | (length list) == 2 = checkIfOpenInputIsCorrect' list
                               | otherwise          = (11,11)

-- | Checks that the input only consists of 2 digits
checkIfOpenInputIsCorrect' :: [Char] -> Pos
checkIfOpenInputIsCorrect' []                 = (13,13)
checkIfOpenInputIsCorrect' (x:xs) | isDigit x = checkIfOpenInputIsCorrect' xs
                                  | otherwise = (11,11)

-- | Modified version that checks that the characteres following F are numbers
findFirst' :: [Char] -> Pos
findFirst' [] = (11,11)
findFirst' (x:xs) | isDigit x = findSecond xs (digitToInt x)
                  | x == 'F' = findFirst' xs
                  | x == 'O' = findFirst' xs
                  | otherwise = (11,11)

-- | Puts a flag at specified position, if that position already contains a flag it will return the previous minesweeper
putFlag :: Minesweeper -> Pos -> Minesweeper
putFlag minesweeper (x,y) | (getValue minesweeper (x,y)) == Nothing = Minesweeper {rows = (!!=) (rows minesweeper) (abs x, row')}
                          |  (n + 100) > 200  = minesweeper
                          | otherwise = Minesweeper {rows = (!!=) (rows minesweeper) (abs x, row'')}
    where row = (!!) (rows minesweeper) (abs x)
          row' = (!!=) row (abs y, (Just 19))
          row'' = (!!=) row (abs y, (Just (n + 100)))
          Just n = (getValue minesweeper (x,y))

-- | Opens a flag at specified position, if that position did not contain a flag it will return the previous minesweeper
openFlag :: Minesweeper -> Pos -> Minesweeper
openFlag minesweeper (x,y) | (getValue minesweeper (x,y)) == Just 19 = Minesweeper {rows = (!!=) (rows minesweeper) (abs x, row')}
                           | (n - 100) < 0  = minesweeper
                           | otherwise = Minesweeper {rows = (!!=) (rows minesweeper) (abs x, row'')}
     where row = (!!) (rows minesweeper) (abs x)
           row' = (!!=) row (abs y, Nothing)
           row'' = (!!=) row (abs y, (Just (n - 100)))
           Just n = (getValue minesweeper (x,y))

-- | A function that takes all the rows in a minesweeper and put them in a single list that can be shuffeled
makeBombs :: StdGen -> Minesweeper
makeBombs g = newMinesweeper (combineRows(shuffle g (concat (rows example))))

-- | A function that returns the new and shuffeled minesweeper
newMinesweeper :: [[Maybe Int]] -> Minesweeper
newMinesweeper minesweeper = Minesweeper {rows = minesweeper}

-- | A function that takes the shuffeled elements and returns 10 new rows from the long list
combineRows :: [Maybe Int] -> [[Maybe Int]]
combineRows [] = [[]]
combineRows list = [take 10 list] ++ combineRows (drop 10 list)

-- | A function that shuffles all the elements in a list
shuffle :: StdGen -> [a] -> [a]
shuffle _ [] = []
shuffle g list  = [list !! number] ++ shuffle g' reducedList
  where
    (number, g') = randomR (0, (length list) - 1) g
    reducedList = removeIndex list number

-- | A function that removes an element in a list at specified index
removeIndex :: [a] -> Int -> [a]
removeIndex [] _                      = []
removeIndex (x:xs) index | index == 0 = xs
                         | otherwise  = [x] ++ removeIndex xs (index-1)

-- | The main function that is called when the game is supposed to start
main :: IO ()
main = do putStrLn "Welcome to Minesweeper Pro"
          g <- newStdGen
          gameLoop (checkNeighbours (makeBombs g) (0,0))

-- | The recursive function that is called uppon when in the game and a move is made, stops if game is won or lost
gameLoop :: Minesweeper -> IO ()
gameLoop minesweeper =
  do printMinesweeper minesweeper "Open a block with xy, put a flag with Fxy and open flags with Oxy"
     pos <- getLine
     if (findFirst pos == (11,11))
       then do putStrLn ("Please choose a correct position")
               gameLoop minesweeper
       else if (findFirst pos == (12,12))
         then gameLoop (putFlag minesweeper (findFirst' pos))
         else if (findFirst pos == (13,13))
           then gameLoop (openFlag minesweeper (findFirst' pos))
           else if gameOver (openCell minesweeper (findFirst pos))
             then finishGame (openCell minesweeper (findFirst pos))
             else if wonGame (openCells minesweeper (findFirst pos))
               then do printMinesweeper (openCells minesweeper (findFirst pos)) "Congratulations, you have beaten Minesweeper Pro!"
             else gameLoop (openCells minesweeper (findFirst pos))

prop_isMinesweeper :: Minesweeper -> Bool
prop_isMinesweeper m = isMinesweeper m

prop_GetValue :: Minesweeper -> Pos -> Bool
prop_GetValue m p@(x,y) = getValue m (x',y') == (rows m !! x') !! y'
  where
    x' = mod (abs x) 10
    y' = mod (abs y) 10
