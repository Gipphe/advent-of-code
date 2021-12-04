module Y2021.Day2 where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Day (Day, SomeDay(..), Task, runTask)
import Y2021.Input (day2Input)

someDay2 :: SomeDay
someDay2 = SomeDay day2

day2 :: Day 2 ()
day2 = do
    runTask day2Task1
    runTask day2Task2

data Instr = InstrUp Int | InstrDown Int | InstrForward Int

parseInstruction :: String -> Maybe Instr
parseInstruction x
    | "forward " `isPrefixOf` x = InstrForward <$> readMaybe (drop 8 x)
    | "up " `isPrefixOf` x      = InstrUp <$> readMaybe (drop 3 x)
    | "down " `isPrefixOf` x    = InstrDown <$> readMaybe (drop 5 x)
    | otherwise                 = Nothing

input :: [Instr]
input =
    fmap (fromMaybe (error "found nothing") . parseInstruction)
        . lines
        $ day2Input

day2Task1 :: Task 1 Int
day2Task1 = pure $ computeTask1 input

computeTask1 :: [Instr] -> Int
computeTask1 = go 0 0
  where
    go d f = \case
        (InstrUp      n : xs) -> go (d - n) f xs
        (InstrDown    n : xs) -> go (d + n) f xs
        (InstrForward n : xs) -> go d (f + n) xs
        []                    -> d * f

day2Task2 :: Task 2 Int
day2Task2 = pure $ computeTask2 input

computeTask2 :: [Instr] -> Int
computeTask2 = go 0 0 0
  where
    go a d p = \case
        (InstrUp      n : xs) -> go (a - n) d p xs
        (InstrDown    n : xs) -> go (a + n) d p xs
        (InstrForward n : xs) -> go a (d + (a * n)) (p + n) xs
        []                    -> d * p
