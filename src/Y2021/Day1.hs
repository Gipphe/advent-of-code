module Y2021.Day1 where

import Day (Day, SomeDay(..), Task, runTask)
import Y2021.Input (day1Input)

input :: [Int]
input = fmap read . lines $ day1Input

someDay1 :: SomeDay
someDay1 = SomeDay day1

day1 :: Day 1 ()
day1 = do
    runTask day1Task1
    runTask day1Task2

day1Task1 :: Task 1 Int
day1Task1 = pure $ computeTask1 input

day1Task2 :: Task 2 Int
day1Task2 = pure $ computeTask2 input

computeTask1 :: [Int] -> Int
computeTask1 (x : y : rest)
    | y > x     = 1 + remaining
    | otherwise = remaining
    where remaining = computeTask1 $ y : rest
computeTask1 _ = 0

computeTask2 :: [Int] -> Int
computeTask2 (a : b : c : d : rest)
    | a + b + c < b + c + d = 1 + remaining
    | otherwise             = remaining
    where remaining = computeTask2 $ b : c : d : rest
computeTask2 _ = 0
