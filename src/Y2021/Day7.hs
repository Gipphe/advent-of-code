module Y2021.Day7 where

import Data.Foldable (foldl')
import Data.List (sort)

import Day (SomeDay(..), Task, runTask)
import Util (split)
import Y2021.Input (day7Input)

someDay7 :: SomeDay
someDay7 = SomeDay @7 do
    runTask day7Task1
    runTask day7Task2

day7Task1 :: Task 1 Int
day7Task1 = pure $ computeTask1 input

day7Task2 :: Task 2 Int
day7Task2 = pure $ computeTask2 input

computeTask1 :: [Int] -> Int
computeTask1 xs = sum $ naiveFuelCost avg <$> xs
    where avg = naiveOptimalPosition xs

computeTask2 :: [Int] -> Int
computeTask2 xs = sum $ complexFuelCost avg <$> xs
    where avg = optimalPosition xs

optimalPosition :: [Int] -> Int
optimalPosition xs = s `div` l
    where (s, l) = foldl' (\(s', l') x -> (s' + x, l' + 1)) (0, 0) xs

naiveOptimalPosition :: [Int] -> Int
naiveOptimalPosition xs = sorted !! middlePoint
  where
    sorted      = sort xs
    middlePoint = length xs `div` 2

naiveFuelCost :: Int -> Int -> Int
naiveFuelCost x y = abs (x - y)

complexFuelCost :: Int -> Int -> Int
complexFuelCost x k = ((kx ^ (2 :: Int)) + abs kx) `div` 2 where kx = k - x

input :: [Int]
input = fmap read . split ',' $ day7Input
