module Main where
import System.IO
import Control.Monad()

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let singlewords = words contents
  let (finalPos, zeros) = zeroCount singlewords (50, 0)
  putStrLn $ "Final position : " ++ show finalPos
  putStrLn $ "Zeros : " ++ show zeros
  hClose handle


adjust :: Int -> Int
adjust n = mod n 100

rotate :: [Char] -> Int -> Int
rotate inst pos
  | head inst == 'L' = adjust (pos - read (tail inst))
  | head inst == 'R' = adjust (pos + read (tail inst))
  | otherwise = pos

zeroCount :: [[Char]] -> (Int,  Int) -> (Int, Int)
zeroCount inst (pos, zeros)
  | null inst = (pos, zeros)
  | rotate (head inst) pos == 0 = zeroCount (tail inst) (0, zeros + 1)
  | otherwise = zeroCount ( tail inst  ) (rotate (head inst) pos, zeros)
