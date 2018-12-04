module Sudoku where

import Data.Maybe
import Data.Char
import Data.List
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
    deriving Show

--A1
-- returns a blank sudoku
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

--A2
-- checks if sudoku is valid
isSudoku :: Sudoku -> Bool
isSudoku sudoku = length (rows sudoku) == 9 && and
    (map (\row -> length row == 9)(rows sudoku))

-- checks if the list elements are real sudoku elements
isElement :: Maybe Int -> Bool
isElement Nothing = True
isElement (Just n) = n `elem` [1..9]

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
readSudoku file = do sudoku <- (readFile file)
                     if isSudoku (stringToSudoku sudoku)
                     then return (stringToSudoku(sudoku))
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
isOkayBlock block = (length (removeNothing block) == length block')
                    where block' = nub(removeNothing block)

--helper for function above, removes Nothing:s
removeNothing :: [Maybe Int] -> [Maybe Int]
removeNothing block = filter (not . isNothing) block

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

blanks :: Sudoku -> [Pos]
blanks sudoku = [(x,y) | (x, row) <- zip[0..8] (rows sudoku),
                         (y, element) <- zip[0..8] row,
                         isNothing element]

prop_blanks_allBlank :: Sudoku -> Bool
prop_blanks_allBlank sudoku = all isNothing $ map (values sudoku) (blanks sudoku)
  where values sudoku (x,y) = (rows sudoku !! x) !! y

--E2
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) []      _             = error "Empty list"
(!!=) (x:xs) (index, value) | length (x:xs) < index = error "Index out of bounds"
(!!=) (x:xs) (0, value)     = value : xs
(!!=) (x:xs) (index, value) = x : xs !!= (index-1, value)

prop_bangBangEquals_correct :: Eq a => [a] -> (Int, a) -> Bool
prop_bangBangEquals_correct [] (_,_)= True
prop_bangBangEquals_correct list (index, value) =
  length list == length (list !!= (index', value)) &&
  (list !!= (index', value)) !! index' == value
    where index' = abs (mod index (length list))
  --((list !! index) not (list !!= (index, value) !! index))


--E3
