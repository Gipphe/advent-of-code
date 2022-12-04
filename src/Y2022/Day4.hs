module Y2022.Day4
    ( someDay4
    , day4Task1
    , computeTask1
    , input
    ) where

import Day (Day, SomeDay(..), Task, runTask)
import Util (split)
import Y2022.Input (day4Input)

input :: [Pair]
input = fmap parsePair . lines $ day4Input

someDay4 :: SomeDay
someDay4 = SomeDay day4

day4 :: Day 4 ()
day4 = do
    runTask day4Task1
    runTask day4Task2

day4Task1 :: Task 1 Int
day4Task1 = pure $ computeTask1 input

day4Task2 :: Task 2 Int
day4Task2 = pure $ computeTask2 input

computeTask1 :: [Pair] -> Int
computeTask1 = length . filter (== True) . fmap oneFullyContainsTheOther

computeTask2 :: [Pair] -> Int
computeTask2 = length . filter (== True) . fmap hasOverlap

data Range = Range Int Int

parseRange :: String -> Range
parseRange s = case split '-' s of
    (start : end : _) -> Range (read start) (read end)
    unknown           -> error $ "invalid range: " <> show unknown

data Pair = Pair Range Range

parsePair :: String -> Pair
parsePair xs = case split ',' xs of
    (left : right : _) -> Pair (parseRange left) (parseRange right)
    unknown            -> error $ "invalid pair: " <> show unknown

oneFullyContainsTheOther :: Pair -> Bool
oneFullyContainsTheOther (Pair (Range l1 r1) (Range l2 r2)) =
    (l1 <= l2 && r2 <= r1) || (l2 <= l1 && r1 <= r2)

hasOverlap :: Pair -> Bool
hasOverlap (Pair (Range l1 r1) (Range l2 r2))
    | r1 < l2   = False
    | r2 < l1   = False
    | otherwise = True
