module Main where
import System.IO
import Control.Monad()
import Data.Char ( digitToInt )

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let batteries = words contents
  let totalJoltage = findTotalJoltage batteries
  putStrLn $ "Total joltage : " ++ show totalJoltage
  hClose handle

findTotalJoltage :: [[Char]] -> Int
findTotalJoltage batteries
  | null batteries = 0
  | otherwise = findBatteryJoltage (head batteries) + findTotalJoltage (tail batteries)

findBatteryJoltage :: [Char] -> Int
findBatteryJoltage battery = do
  let (tensIndex, tens) = findHighest (init battery)
  let (_, batteryTails) = splitAt (tensIndex +1) battery
  let (_, units) = findHighest batteryTails
  10 * tens + units

findHighest :: [Char] -> (Int, Int)
findHighest subBattery = findHighestInner subBattery 0 0 0

findHighestInner :: [Char] -> Int -> Int -> Int -> (Int, Int)
findHighestInner battery currentIndex currentMaxIndex currentMax
  | null battery = (currentMaxIndex, currentMax)
  | currentMax < currentVal = findHighestInner (tail battery) (currentIndex + 1) currentIndex currentVal
  | otherwise = findHighestInner (tail battery) (currentIndex + 1) currentMaxIndex currentMax
  where
    currentVal = digitToInt (head battery)
