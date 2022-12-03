module Y2022.Day1
    ( someDay1
    , day1Task1
    , computeTask1
    , input
    ) where

import Data.List (sort)
import Data.Ord (Down(..))
import Day (Day, SomeDay(..), Task, runTask)
import Util (splitOnDoubleNewline)
import Y2022.Input (day1Input)

input :: [[Int]]
input = fmap (fmap read . lines) . splitOnDoubleNewline $ day1Input

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

computeTask1 :: [[Int]] -> Int
computeTask1 = maximum . fmap sum

computeTask2 :: [[Int]] -> Int
computeTask2 = getDown . sum . take 3 . sort . fmap (Down . sum)
