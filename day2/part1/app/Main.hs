module Main where
import System.IO
import Debug.Trace
import Data.List.Split

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let idRanges = splitOn "," contents
  let sumOfWrongIds = trace (show idRanges) (findWrongIds idRanges)
  putStrLn $ "Sum of wrong IDs : " ++ show sumOfWrongIds
  hClose handle

findWrongIds :: [[Char]] -> Int
findWrongIds ranges
  | null ranges = 0
  | otherwise = sumOfWrongIdsInRange (head ranges) + findWrongIds (tail ranges)

sumOfWrongIdsInRange :: [Char] -> Int
sumOfWrongIdsInRange range = do
    let limit = splitOn "-" range
    let minId = head limit
    let maxId =
          if '\n' == last lastLim then init lastLim
          else lastLim
          where lastLim = last limit
    trace ("Range : " ++ range) (goThroughRange minId maxId)


goThroughRange :: [Char] -> [Char] -> Int
goThroughRange minId maxId
  | minId == maxId = trace "minId == maxId" isWrongId maxId
  | otherwise = trace ("check : " ++ minId ++ " new range : " ++ newMinId ++ "-" ++ maxId ++";") isWrongId minId + goThroughRange newMinId maxId
  where
    newMinId = show (read minId + 1)

isWrongId :: [Char] -> Int
isWrongId testedId
  | odd testedIdLength = 0
  | otherwise = isWrongIdInner testedId testedIdLength
  where
    testedIdLength = length testedId

isWrongIdInner :: [Char] -> Int -> Int
isWrongIdInner testedId testedIdLength
  | firstHalf == secondHalf = read testedId
  | otherwise = 0
  where
    (firstHalf, secondHalf) = trace ("testedId :" ++ testedId) (splitAt (quot testedIdLength 2) testedId)
