module Main where
import System.IO
import Data.List.Split (splitOn)
import Debug.Trace

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let inputData = splitOn "\n\n" contents
  let ranges = parseRanges (lines (head inputData))
  let ingredients = parseIngredients (lines (last inputData))
  let (freshIngredients, quantityOfFreshIngredients) = checkForFreshIngredients ranges ingredients
  -- putStrLn $ "Sum of wrong IDs : " ++ show quantityOfFreshIngredients
  trace (show freshIngredients ++ "\n-----\n" ++ show quantityOfFreshIngredients) hClose handle

parseRanges :: [String] -> [(Int, Int)]
parseRanges rs
  | null rs = []
  | otherwise = (read (head splitS), read (last splitS)) : parseRanges (tail rs)
  where
    splitS = splitOn "-" (head rs)

parseIngredients :: [String] -> [Int]
parseIngredients is
  | null is = []
  | otherwise = read (head is) : parseIngredients (tail is)

checkForFreshIngredients :: [(Int, Int)] -> [Int] -> ([Int], Int)
checkForFreshIngredients rs is
  | null is = ([], 0)
  | f > 0 = (f : nxtFs, 1 + nxtIs)
  | otherwise = (nxtFs, nxtIs)
  where
    f = isFresh rs (head is)
    (nxtFs, nxtIs) = checkForFreshIngredients rs (tail is)

isFresh :: [(Int, Int)] -> Int -> Int
isFresh rs i
  | null rs = 0
  | f > 0 = f
  | otherwise = isFresh (tail rs) i
  where
    f = isFreshInner (head rs) i

isFreshInner ::(Int, Int) -> Int -> Int
isFreshInner (l, h) i
  | i < l = 0
  | i > h = 0
  | otherwise = i
