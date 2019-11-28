module Sudoku where

import Data.Maybe
import Data.Char
import Data.List hiding (findIndices)
import Test.QuickCheck

-------------------------------------------------------------------
--Examples
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

----------------------------------A--------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
    deriving (Show, Eq)

--A1
-- returns a blank sudoku
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

--A2
-- checks if sudoku is valid
isSudoku :: Sudoku -> Bool
isSudoku sudoku = length (rows sudoku) == 9 && and
    (map (\row -> length row == 9)(rows sudoku))

--A3
-- checks if a sudoku is completed
isFilled :: Sudoku -> Bool
isFilled sudoku = isFilled sudoku && all isRowFilled (rows sudoku)
  where
    isRowFilled = all isJust

------------------------------------B----------------------------------------
--B1
-- prints out the sudoku
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = putStrLn (unlines (map makeAString (rows sudoku)))

-- Makes a string out of the list elements
makeAString :: [Maybe Int] -> [Char]
makeAString row = map makeChar row

-- turns the elements into characters
makeChar :: Maybe Int -> Char
makeChar Nothing = '.'
makeChar (Just n) = intToDigit n

--B2
-- reads the sudoku from a file
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do sudoku <- readFile file
                     if isSudoku (stringToSudoku sudoku)
                     then return (stringToSudoku sudoku)
                     else error "Not a sudoku"

--turns the string from the file into a valid sudoku
stringToSudoku :: String -> Sudoku
stringToSudoku string = Sudoku (map transformFile (lines string))

--helper for above function
transformFile :: String -> [Maybe Int]
transformFile string = map makeMaybeInt string

--same as above
makeMaybeInt :: Char -> Maybe Int
makeMaybeInt char
        | char == '.' = Nothing
        | char `elem` "123456789" = Just (digitToInt char)

--------------------------------C---------------------------------
--C1
--creates sudoku cells
cell :: Gen (Maybe Int)
cell = frequency [(7, return Nothing), (3, do n <- choose (1,9)
                                              return (Just n))]

--C2
-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
    arbitrary =
      do rows <- vectorOf 9 (vectorOf 9 cell)
         return (Sudoku rows)

--C3
--tests if it is a sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudoku = isSudoku sudoku

--------------------------------------D-------------------------------------

type Block = [Maybe Int]

--D1
--checks if a block is okay
isOkayBlock :: Block -> Bool
isOkayBlock block = length (removeNothing block) == length block'
                    where block' = nub(removeNothing block)

--helper for function above, removes Nothing:s
removeNothing :: [Maybe Int] -> [Maybe Int]
removeNothing block = filter isJust block

--D2
--makes blocks
blocks :: Sudoku -> [Block]
blocks sudoku = [] ++ rows sudoku ++ toCols sudoku ++ toBlocks sudoku

--makes columns to rows
toCols :: Sudoku -> [[Maybe Int]]
toCols sudoku = transpose (rows sudoku)

--helper for function above, makes the 3x3 squares
toBlocks :: Sudoku -> [[Maybe Int]]
toBlocks sudoku = []
    ++ firstBlock (take 3 (rows sudoku))
    ++ secondBlock (take 3 (rows sudoku))
    ++ thirdBlock (take 3 (rows sudoku))
    ++ firstBlock (slice' 3 6 (rows sudoku))
    ++ secondBlock (slice' 3 6 (rows sudoku))
    ++ thirdBlock (slice' 3 6 (rows sudoku))
    ++ firstBlock (drop 6 (rows sudoku))
    ++ secondBlock (drop 6 (rows sudoku))
    ++ thirdBlock (drop 6 (rows sudoku))

--helps the helper function
firstBlock :: [[Maybe Int]] -> [[Maybe Int]]
firstBlock rows = [concat ([] ++ map (take 3) rows)]

--helps the helper function
secondBlock :: [[Maybe Int]] -> [[Maybe Int]]
secondBlock rows = [concat ([] ++ map (slice 3 6) rows)]

--helps the helper function
thirdBlock :: [[Maybe Int]] -> [[Maybe Int]]
thirdBlock rows = [concat ([] ++ map (drop 6) rows)]

--helps the helper function
slice :: Int -> Int -> [Maybe Int] -> [Maybe Int]
slice from to list = take (to - from) (drop from list)

--helps the helper function
slice' :: Int -> Int -> [[Maybe Int]] -> [[Maybe Int]]
slice' from to list = take (to - from) (drop from list)

--tests if blocks in sudoku is valid
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudoku = length blocks' == 27 && all (\b -> length b == 9) blocks'
        where blocks' = blocks sudoku

--D3
-- checks if sudoku is a valid sudoku
isOkay :: Sudoku -> Bool
isOkay sudoku = all isOkayBlock (blocks sudoku)

--E1
type Pos = (Int,Int)

-- | A function that gives all blank spaces in a sudoku
blanks :: Sudoku -> [Pos]
blanks sudoku = [(x,y) | (x, row) <- zip[0..8] (rows sudoku),
                         (y, element) <- zip[0..8] row,
                         isNothing element]

-- | A function that checks that the function blans does twhat it is supposed to do
prop_blanks_allBlank :: Sudoku -> Bool
prop_blanks_allBlank sudoku = all isNothing $ map (values sudoku) (blanks sudoku)
  where values sudoku (x,y) = (rows sudoku !! x) !! y

--E2
-- | An operator that changes a value at a specific index to a new one
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) []      _             = error "Empty list"
(!!=) (x:xs) (index, value) | length (x:xs) < index = error "Index out of bounds"
(!!=) (x:xs) (0, value)     = value : xs
(!!=) (x:xs) (index, value) = x : xs !!= (index-1, value)

-- | A funtion that checks that the operator (!!=) does what it is supposed to do
prop_bangBangEquals_correct :: Eq a => [a] -> (Int, a) -> Bool
prop_bangBangEquals_correct [] (_,_)= True
prop_bangBangEquals_correct list (index, value) =
  length list == length (list !!= (index', value)) &&
  (list !!= (index', value)) !! index' == value
    where index' = abs (mod index (length list))

--E3
-- | A funtcion that updates a position in the sudoku with a new value
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sudoku (x, y) value = Sudoku {rows = (!!=) (rows sudoku) (abs x, row')}
    where row = (!!) (rows sudoku) (abs x)
          row' = (!!=) row (abs y, value)

-- | A function that checks if the function update does what it is supposed to do
prop_update_updated :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update_updated sudoku (x,y) value = pos == value
    where row = (!!) (rows sudoku') (abs x')
          pos = (!!) row (abs y')
          sudoku' = update sudoku (x',y') value
          x' = mod x 8
          y' = mod y 8

--E4
-- | A function that starts the work for getting the candidates that can fill the blank spots
candidates :: Sudoku -> Pos -> [Int]
candidates sudoku (x,y) | isJust pos' = []
                        | otherwise = getCandidates (extractInts (row ++ col ++ block)) [1..9]
  where
    pos   = rows' !! (abs x)
    pos'  = pos !! (abs y)
    rows' = rows sudoku
    row   = rows' !! x
    col   = (transpose rows') !! y
    block = getBlock sudoku (x,y)

-- | A function that gets the block that the position is in
getBlock :: Sudoku -> Pos -> [Maybe Int]
getBlock sudoku (x,y)
  | x `elem` [0..2] && y `elem` [0..2] = getValues $ firstBlock (take 3 (rows sudoku))
  | x `elem` [0..2] && y `elem` [3..5] = getValues $ secondBlock (take 3 (rows sudoku))
  | x `elem` [0..2] && y `elem` [6..8] = getValues $ thirdBlock (take 3 (rows sudoku))
  | x `elem` [3..5] && y `elem` [0..2] = getValues $ firstBlock (slice' 3 6 (rows sudoku))
  | x `elem` [3..5] && y `elem` [3..5] = getValues $ secondBlock (slice' 3 6 (rows sudoku))
  | x `elem` [3..5] && y `elem` [6..8] = getValues $ thirdBlock (slice' 3 6 (rows sudoku))
  | x `elem` [6..8] && y `elem` [0..2] = getValues $ firstBlock (drop 6 (rows sudoku))
  | x `elem` [6..8] && y `elem` [3..5] = getValues $ secondBlock (drop 6 (rows sudoku))
  | x `elem` [6..8] && y `elem` [6..8] = getValues $ thirdBlock (drop 6 (rows sudoku))

-- | A function that takes all Just Ints and removes the Nothings
getValues :: [[Maybe Int]] -> [Maybe Int]
getValues [[]] = []
getValues [(x:xs)] | isJust x = x : getValues [xs]
                   | otherwise = getValues [xs]

-- | A function that extracts all Ints from Maybe Ints
extractInts :: [Maybe Int] -> [Int]
extractInts []            = []
extractInts (Nothing:xs)  = extractInts xs
extractInts (Just x:xs)   = x : extractInts xs

-- | A function that returns all candidates for a position in the sudoku
getCandidates :: [Int] -> [Int] -> [Int]
getCandidates _ [] = []
getCandidates list (x:xs) | x `notElem` list = x : getCandidates list xs
                          | otherwise  = getCandidates list xs

--F1
-- | A funtion that checks if a variable is a sudoku and if it is valid, then solves it
solve :: Sudoku -> Maybe Sudoku
solve sudoku | not $ isSudoku sudoku = Nothing
             | not $ isOkay sudoku = Nothing
             | otherwise = solve' sudoku (blanks sudoku)

-- | A function that does the hard work for solving a sudoku using recursion
solve' :: Sudoku -> [Pos] -> Maybe Sudoku
solve' sudoku []      = Just sudoku
solve' sudoku (x:xs)  = listToMaybe $ catMaybes currentSudoku
  where
    candidate = candidates sudoku x
    currentSudoku = [solve' (update sudoku x $ Just candidate') xs | candidate' <- candidate]

--F2
-- | A function that reads a file and then solves the file if it is a sudoku
readAndSolve :: FilePath -> IO ()
readAndSolve file = do sudoku <- readSudoku file
                       let solved = solve sudoku
                       printSudoku (fromJust solved)

--F3
-- | A function that checks if a solved sudoku is a solution for another sudoku
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf solvedSudoku toSolve = Just solvedSudoku == solve toSolve

--F4
-- | A function that tests if the solutions are valid
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sudoku = isJust (solve sudoku) ==> fromJust (solve sudoku) `isSolutionOf` sudoku


--Problem 1
findIndices :: (a->Bool) -> [a] -> [Int]
findIndices f list = fi2 f list 0 []

fi2 :: (a -> Bool) -> [a] -> Int -> [Int] -> [Int]
fi2 _ [] _ list = list
fi2 f (x:xs) index list | f x == True = fi2 f xs (index + 1) (list ++ [index])
                        | otherwise = fi2 f xs (index + 1) list
