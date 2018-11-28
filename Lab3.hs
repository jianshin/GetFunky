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
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

--A2
isSudoku :: Sudoku -> Bool
isSudoku sudoku = length (rows sudoku) == 9 && and
    (map (\row -> length row == 9)(rows sudoku))

        --(map (isElement) row)
isElement :: Maybe Int -> Bool
isElement Nothing = True
isElement (Just n) = elem n [1..9]

--A3
isFilled :: Sudoku -> Bool
isFilled sudoku = isFilled sudoku && all isRowFilled (rows sudoku)
  where
    isRowFilled = all (not . isNothing)

------------------------------------B----------------------------------------
--B1
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = putStrLn (unlines (map makeAString (rows sudoku)))

makeAString :: [Maybe Int] -> [Char]
makeAString row = map makeChar row

makeChar :: Maybe Int -> Char
makeChar Nothing = '.'
makeChar (Just n) = intToDigit n

--B2
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do sudoku <- (readFile file)
                     if isSudoku (stringToSudoku sudoku)
                     then return (stringToSudoku(sudoku))
                     else error "Not a sudoku"

stringToSudoku :: String -> Sudoku
stringToSudoku string = Sudoku (map transformFile (lines string))

transformFile :: String -> [Maybe Int]
transformFile string = map makeMaybeInt string

makeMaybeInt :: Char -> Maybe Int
makeMaybeInt char 
        | char == '.' = Nothing
        | char `elem` "123456789" = Just (digitToInt char)

--------------------------------C---------------------------------
--C1
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
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudoku = isSudoku sudoku

--------------------------------------D-------------------------------------

type Block = [Maybe Int]

--D1
isOkayBlock :: Block -> Bool
isOkayBlock block = (length (removeNothing block) == length block')
                    where block' = nub(removeNothing block)
                      
removeNothing :: [Maybe Int] -> [Maybe Int]
removeNothing block = filter (not . isNothing) block
                            
--D2
blocks :: Sudoku -> [Block]
blocks sudoku = [] ++ rows sudoku ++ toCols sudoku ++ toBlocks sudoku

toCols :: Sudoku -> [[Maybe Int]]
toCols sudoku = transpose (rows sudoku)

toBlocks :: Sudoku -> [[Maybe Int]]
toBlocks sudoku = [] ++ firstBlock (take 3 (rows sudoku)) ++ secondBlock (take 3 (rows sudoku)) ++ thirdBlock (take 3 (rows sudoku))
        ++ firstBlock (slice' 3 6 (rows sudoku)) ++ secondBlock (slice' 3 6 (rows sudoku)) ++ thirdBlock (slice' 3 6 (rows sudoku)) 
        ++ firstBlock (drop 6 (rows sudoku)) ++ secondBlock (drop 6 (rows sudoku)) ++ thirdBlock (drop 6 (rows sudoku))

firstBlock :: [[Maybe Int]] -> [[Maybe Int]] 
firstBlock rows = [concat ([] ++ map (take 3) rows)]

secondBlock :: [[Maybe Int]] -> [[Maybe Int]]
secondBlock rows = [concat ([] ++ map (slice 3 6) rows)]

thirdBlock :: [[Maybe Int]] -> [[Maybe Int]]
thirdBlock rows = [concat ([] ++ map (drop 6) rows)]

slice :: Int -> Int -> [Maybe Int] -> [Maybe Int]
slice from to list = take (to - from) (drop from list)

slice' :: Int -> Int -> [[Maybe Int]] -> [[Maybe Int]]
slice' from to list = take (to - from) (drop from list)

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudoku = length blocks' == 27 && all (\b -> length b == 9) blocks'
        where blocks' = blocks sudoku

--D3
isOkay :: Sudoku -> Bool
isOkay sudoku = all isOkayBlock (blocks sudoku)
