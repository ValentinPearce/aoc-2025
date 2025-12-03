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
  | otherwise = read (findBatteryJoltage 12 (head batteries) ++ show (findTotalJoltage (tail batteries)))

findBatteryJoltage :: Int -> [Char] -> [Char]
findBatteryJoltage size battery = do
  let (batteryHead, _) = splitAt (length battery - size) battery
  let (tensIndex, tens) = findHighest batteryHead
  let (_, batteryTails) = splitAt (tensIndex +1) battery
  let units = findBatteryJoltage (size - 1) batteryTails
  read (tens ++ units)

findHighest :: [Char] -> (Int, [Char])
findHighest subBattery = findHighestInner subBattery 0 0 0

findHighestInner :: [Char] -> Int -> Int -> Int -> (Int, [Char])
findHighestInner battery currentIndex currentMaxIndex currentMax
  | null battery = (currentMaxIndex, show currentMax)
  | currentVal == 9 = (currentIndex, show currentVal)
  | currentMax < currentVal = findHighestInner (tail battery) (currentIndex + 1) currentIndex currentVal
  | otherwise = findHighestInner (tail battery) (currentIndex + 1) currentMaxIndex currentMax
  where
    currentVal = digitToInt (head battery)
