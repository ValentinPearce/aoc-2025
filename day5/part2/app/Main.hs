module Main where
import System.IO
import Data.List (nub)
import Data.List.Split (splitOn)
import Debug.Trace

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let inputData = splitOn "\n\n" contents
  let ranges = parseRanges (lines (head inputData))
  -- putStrLn $ "Sum of wrong IDs : " ++ show quantityOfFreshIngredients
  let freshIds = calculateFreshIds ranges
  trace ("\nFresh Id Count = " ++ show freshIds) hClose handle

parseRanges :: [String] -> [(Int, Int)]
parseRanges rs
  | null rs = []
  | otherwise = (read (head splitS), read (last splitS)) : parseRanges (tail rs)
  where
    splitS = splitOn "-" (head rs)

calculateFreshIds :: [(Int, Int)] -> Int
calculateFreshIds rs
  | null rs = 0
  | otherwise = trace ("Condensed : " ++ show crs) rangesToAmounts crs
  where
    crs = condenseRanges rs

rangesToAmounts :: [(Int, Int)] -> Int
rangesToAmounts rs
  | null rs = 0
  | otherwise = rmax - rmin + 1 + rangesToAmounts (tail rs)
  where
    (rmin, rmax) = head rs

condenseRanges :: [(Int, Int)] -> [(Int, Int)]
condenseRanges rs
  | null rs = []
  | otherwise = trace ("nrs" ++ show nrs) ks
  where
    (ks, nrs ) = condenseRangesInner [] rs

condenseRangesInner :: [(Int, Int)] -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
condenseRangesInner ks rs
  | null rs = (ks, rs)
  | null ks = (knk, rnk)
  | rmin <= kmin && kmax <= rmax = condenseRangesInner ((rmin, rmax) : tail ks) (tail rs)
  | rmin >= kmin && kmax >= rmax = condenseRangesInner ks (tail rs)
  | rmax >= kmin && kmax >= rmax = condenseRangesInner ((rmin, kmax) : tail ks) (tail rs)
  | rmin >= kmin && kmax >= rmin = condenseRangesInner ((kmin, rmax) : tail ks) (tail rs)
  | otherwise = condenseRangesInner ((kmin, kmax) : knext) rnext
  where
    (kmin, kmax) = head ks
    (rmin, rmax) = trace ("Keep : " ++ show ks ++ "\nTest : " ++ show rs) head rs
    (knext, rnext) = trace "otherwise\n" condenseRangesInner (tail ks) rs
    (knk, rnk) = trace "nk" condenseRangesInner [head rs] (tail rs)
--- Everything nuder here theoretically works (works on the test data) but I don't know how long it takes
rangesToInts :: [(Int, Int)] -> [Int]
rangesToInts rs
  | null rs = []
  | otherwise = trace ("List length : " ++ show (length rs)) combineIntLists (rangesToInts (tail rs)) (rangeToInts (head rs))

rangeToInts :: (Int, Int) -> [Int]
rangeToInts (rmin, rmax)
  | rmin == rmax = [rmin]
  | otherwise = rmin : rangeToInts (rmin + 1, rmax)

combineIntLists :: [Int] -> [Int] -> [Int]
combineIntLists l1 l2 = nub (l1 ++ l2)
