module Main where
import System.IO
import Debug.Trace

main :: IO ()
main = do
  handle <- openFile "example.txt" ReadMode
  contents <- hGetContents handle
  let fileLines = words contents
  let count = countNeighboursFull (findAccessibleRolls fileLines)
  putStrLn $ "Accessible Rolls : " ++ show count
  hClose handle

findAccessibleRolls :: [[Char]] -> [[Int]]
findAccessibleRolls fileLines
  | null fileLines = [[0]]
  | otherwise = linesToNumGrid fileLines '@'

linesToNumGrid :: [[Char]] -> Char -> [[Int]]
linesToNumGrid fileLines c
  | length fileLines == 1 = [lineToNumLine (head fileLines) c]
  | otherwise = lineToNumLine (head fileLines) c : linesToNumGrid (tail fileLines) c

lineToNumLine :: [Char] -> Char -> [Int]
lineToNumLine line c
  | length line == 1 = [charToInt (head line) c]
  | otherwise = charToInt (head line) c : lineToNumLine (tail line) c

charToInt :: Char -> Char -> Int
charToInt toConvert convertIf
  | toConvert == convertIf = 1
  | otherwise = 0

padRow :: [Int] -> [Int]
padRow r = 0 : r ++ [0]

zeRow :: Int -> [Int]
zeRow l
  | l == 1 = [0]
  | otherwise = 0 : zeRow (l - 1)

countNeighboursFull :: [[Int]] -> (Int, Int)
countNeighboursFull g 
  | null g = trace (show g ++ "\n0,0") (0, 0)
  | processedRem /= 0 = trace (show g ++ "\n" ++ show (1+nxt) ++ "," ++ show (processedRem + nxtRem)) (1 + nxt, processedRem + nxtRem)
  | otherwise = trace (show g ++ "\n" ++ "1," ++ show processedRem) (1, processedRem)
  where
    rows = length g
    cols = length (head g)
    gPadded = [zeRow (cols + 2)] ++ map padRow g ++ [zeRow (cols + 2)]
    (processed, processedRem) = countNeighbours gPadded rows cols
    (nxt, nxtRem) = countNeighboursFull (unPad processed)

countNeighbours :: [[Int]] -> Int -> Int -> ([[Int]], Int)
countNeighbours g r c
  | r == 0 = ([[0]], 0)
  | otherwise = ( cfrTab : cnrTab , cfrRem + cnrRem)
  where
    (cfrTab, cfrRem) = countForRow g r c
    (cnrTab, cnrRem) = countNeighbours g (r-1) c

countForRow :: [[Int]] -> Int -> Int -> ([Int], Int)
countForRow g r c
  | c == 0 = ([0], 0)
  | otherwise = (has4nb : nextCol, removed + nextColRemoved)
  where
    nbs = calc g r c
    (has4nb, removed) = check 4 nbs
    (nextCol, nextColRemoved) = countForRow g r (c-1)

calc :: [[Int]] -> Int -> Int -> Int
calc g r c
  | g !! r !! c == 1 = (g !! (r - 1) !! (c - 1)) + (g !! (r - 1) !! c) + (g !! (r - 1) !! (c + 1))
                     + (g !! r       !! (c - 1))                       + (g !! r       !! (c + 1))
                     + (g !! (r + 1) !! (c - 1)) + (g !! (r + 1) !! c) + (g !! (r + 1) !! (c + 1))
  | otherwise = -1

check :: Int -> Int -> (Int, Int)
check limit val
  | val == -1 = (0, 0)
  | val < limit = (0, 1)
  | otherwise = (1, 0)

unPad :: [[Int]] -> [[Int]]
unPad g = init (tail (unPadLines g))

unPadLines :: [[Int]] -> [[Int]]
unPadLines g
  | length g == 1 = [init (tail (head g))]
  | otherwise = init (tail (head g)) : unPadLines (tail g)
