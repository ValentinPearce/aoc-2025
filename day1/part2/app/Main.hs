module Main where
import System.IO
import Control.Monad()
import Debug.Trace

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let singlewords = words contents
  let (finalPos, zeros) = zeroCount singlewords 50 0
  putStrLn $ "Final position : " ++ show finalPos
  putStrLn $ "Zeros : " ++ show zeros
  hClose handle


adjustAndCount :: Int -> Int -> (Int, Int)
adjustAndCount o n
  | n == 0 = (n, 1)
  | n < 0 && o /= 0 = (mod n 100, 1 + abs (quot n 100))
  | n > 0 && o < 0 = (mod n 100, 1 + quot n 100)
  | otherwise = (mod n 100, abs (quot n 100))

rotate :: [Char] -> Int -> (Int, Int)
rotate instruction pos
  | head instruction == 'L' = adjustAndCount pos (pos - read (tail instruction))
  | head instruction == 'R' = adjustAndCount pos (pos + read (tail instruction))
  | otherwise = (pos, 0)

zeroCount :: [[Char]] -> Int -> Int -> (Int, Int)
zeroCount inst pos zeros
  | null inst = (pos, zeros)
  | otherwise = trace (show pos ++ "/" ++ show zeros) (zeroCount (tail inst) newPos (zeros + addedZeros))
  where
    (newPos, addedZeros) = rotate (head inst) pos
