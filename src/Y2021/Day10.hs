module Y2021.Day10
    ( someDay10
    , day10Task1
    , day10Task2
    , computeTask1
    , computeTask2
    , input
    ) where

import Data.Foldable (foldl')
import Data.Maybe (isNothing, mapMaybe)

import Day (SomeDay(SomeDay), Task, runTask)
import Util (median)
import Y2021.Input (day10Input)

someDay10 :: SomeDay
someDay10 = SomeDay @10 do
    runTask day10Task1
    runTask day10Task2

day10Task1 :: Task 1 Int
day10Task1 = pure $ computeTask1 input

day10Task2 :: Task 2 Int
day10Task2 = pure $ computeTask2 input

computeTask1 :: [String] -> Int
computeTask1 = sum . mapMaybe findLineError

computeTask2 :: [String] -> Int
computeTask2 =
    median
        . fmap (findMissingCloser . fst)
        . filter (\(_, x) -> isNothing x)
        . fmap (\x -> (x, findLineError x))

findLineError :: String -> Maybe Int
findLineError = go []
  where
    go _     []       = Nothing
    go stack (x : xs) = case x of
        '(' -> go (')' : stack) xs
        '[' -> go (']' : stack) xs
        '<' -> go ('>' : stack) xs
        '{' -> go ('}' : stack) xs
        c   -> findInStack c
      where
        findInStack c = case stack of
            [] -> Nothing
            (y : stack')
                | y == c    -> go stack' xs
                | otherwise -> Just $ corruptScore c

corruptScore :: Char -> Int
corruptScore = \case
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _   -> 0

findMissingCloser :: String -> Int
findMissingCloser = go []
  where
    go stack []       = stackScore stack
    go stack (x : xs) = case x of
        '(' -> go (')' : stack) xs
        '[' -> go (']' : stack) xs
        '<' -> go ('>' : stack) xs
        '{' -> go ('}' : stack) xs
        c   -> findInStack c
      where
        findInStack c = case stack of
            [] -> stackScore stack
            (y : stack')
                | y == c    -> go stack' xs
                | otherwise -> stackScore (y : stack')
    stackScore = foldl' (flip addMissingScore) 0


addMissingScore :: Char -> Int -> Int
addMissingScore c x = x * 5 + case c of
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4
    _   -> 0

input :: [String]
input = filter (not . null) . lines $ day10Input
