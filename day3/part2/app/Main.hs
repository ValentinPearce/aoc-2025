module Main where
import System.IO
import Control.Monad()
import Data.Char ( digitToInt )
import Debug.Trace

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
  | otherwise = read (findBatteryJoltage 12 (head batteries)) + findTotalJoltage (tail batteries)

findBatteryJoltage :: Int -> [Char] -> [Char]
findBatteryJoltage size battery
  | size <=0 = ""
  | null battery = ""
  | size == 1 = highest
  | otherwise = trace ("findBatteryJoltage : " ++ battery) (
  let (batteryHead, _) = splitAt (length battery - size + 1) battery
      (tensIndex, tens) = findHighest batteryHead
      (_, batteryTails) = splitAt (tensIndex +1) battery
      units = findBatteryJoltage (size - 1) batteryTails
  in (tens ++ units))
  where
    (_, highest) = findHighest battery

findHighest :: [Char] -> (Int, [Char])
findHighest subBattery = trace ("findHighest : " ++ subBattery) (findHighestInner subBattery 0 0 0)

findHighestInner :: [Char] -> Int -> Int -> Int -> (Int, [Char])
findHighestInner battery currentIndex currentMaxIndex currentMax
  | null battery = (currentMaxIndex, show currentMax)
  | currentVal == 9 = (currentIndex, show currentVal)
  | currentMax < currentVal = findHighestInner (tail battery) (currentIndex + 1) currentIndex currentVal
  | otherwise = findHighestInner (tail battery) (currentIndex + 1) currentMaxIndex currentMax
  where
    currentVal = digitToInt (head battery)
