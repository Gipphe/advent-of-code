module Y2021.Day3
    ( someDay3
    , day3Task1
    , day3Task2
    , computeTask1
    , computeTask2
    , input
    ) where

import Control.Arrow ((&&&))
import Data.Foldable (foldl')
import Data.List (transpose)
import GHC.Float (int2Double)

import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Day (Day, SomeDay(..), Task, runTask)
import Y2021.Input (day3Input)

someDay3 :: SomeDay
someDay3 = SomeDay day3

day3 :: Day 3 ()
day3 = do
    runTask day3Task1
    runTask day3Task2

day3Task1 :: Task 1 Int
day3Task1 = pure $ computeTask1 input

day3Task2 :: Task 2 Int
day3Task2 = pure $ computeTask2 input

computeTask1 :: [[Int]] -> Int
computeTask1 =
    uncurry (*)
        . bimap mergeBinary mergeBinary
        . unzip
        . fmap ((gammaRate &&& epsilonRate) . average)
        . transpose

computeTask2 :: [[Int]] -> Int
computeTask2 xs = oxy * co2
  where
    oxy = mergeBinary $ computeDumb gammaRate xs
    co2 = mergeBinary $ computeDumb epsilonRate xs

computeDumb :: (Double -> Int) -> [[Int]] -> [Int]
computeDumb decide = go 0
  where
    go _   []  = error "computeDumb: empty list"
    go _   [x] = x
    go idx xs  = go (idx + 1) $ filter (\x -> x !! idx == ref) xs
        where ref = decide . average $ transpose xs !! idx

average :: [Int] -> Double
average xs
    | null xs
    = error "empty list"
    | otherwise
    = uncurry (/)
        $ foldl' (\(!total, !count) x -> (total + x, count + 1)) (0, 0)
        $ fmap int2Double xs

gammaRate :: Double -> Int
gammaRate x
    | x >= 0.5  = 1
    | otherwise = 0

epsilonRate :: Double -> Int
epsilonRate x
    | x >= 0.5  = 0
    | otherwise = 1

mergeBinary :: [Int] -> Int
mergeBinary = go 0 . reverse
  where
    go :: Int -> [Int] -> Int
    go n (x : xs) = x * 2 ^ n + go (n + 1) xs
    go _ []       = 0

input :: [[Int]]
input = fmap (fmap digitToInt) . lines $ day3Input
