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
  | minId == maxId = trace "minId == maxId" isWrongId primes maxId
  | otherwise = trace ("check : " ++ minId ++ " new range : " ++ newMinId ++ "-" ++ maxId ++";") isWrongId primes minId + goThroughRange newMinId maxId
  where
    newMinId = show (read minId + 1)
    primes = [11, 7, 5, 3, 2]


isWrongId :: [Int] -> [Char] -> Int
isWrongId primes testedId
  | null primes = 0
  | mod testedIdLength prime == 0 = isWrongIdInner primes testedId testedIdLength
  | otherwise = isWrongId (tail primes) testedId
  where
    testedIdLength = length testedId
    prime = head primes

isWrongIdInner :: [Int] -> [Char] -> Int -> Int
isWrongIdInner primes testedId testedIdLength
  | checkSegments segments = read testedId
  | otherwise = isWrongId (tail primes) testedId
  where
    chunkSize = quot testedIdLength (head primes)
    segments = chunksOf chunkSize testedId

checkSegments :: [[Char]] -> Bool
checkSegments segments
  | length segments == 1 = True
  | segments !! 1 == head segments = checkSegments (tail segments)
  | otherwise = False
