module Y2022.Day6
    ( someDay6
    , computeTask1
    , computeTask2
    , input
    ) where

import Data.Containers.ListUtils (nubIntOn)
import Day (Day, SomeDay(..), Task, runTask)
import Util (trim)
import Y2022.Input (day6Input)

input :: String
input = trim day6Input

someDay6 :: SomeDay
someDay6 = SomeDay day6

day6 :: Day 6 ()
day6 = do
    runTask day6Task1
    runTask day6Task2

day6Task1 :: Task 1 Int
day6Task1 = pure $ computeTask1 input

day6Task2 :: Task 2 Int
day6Task2 = pure $ computeTask2 input

computeTask1 :: String -> Int
computeTask1 = length . findMarker 4 ""

computeTask2 :: String -> Int
computeTask2 = length . findMarker 14 ""

findMarker :: Int -> String -> String -> String
findMarker n coll xs
    | length (nubIntOn fromEnum test) == length test = test <> coll
    | otherwise = findMarker n newColl rest
  where
    test      = take n xs
    (h, rest) = splitAt 1 xs
    newColl   = h <> coll
