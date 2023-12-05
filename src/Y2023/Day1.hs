module Y2023.Day1
  ( someDay1,
    day1Task1,
    day1Task2,
    computeTask1,
    computeTask2,
    input,
  )
where

import Control.Applicative (asum)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isDigit)
import Data.List (foldl', isPrefixOf)
import Data.Maybe (maybeToList)
import Day (SomeDay (..), Task, runTask)
import Util (trim)
import Y2023.Input (day1Input)

someDay1 :: SomeDay
someDay1 = SomeDay @1 do
  runTask day1Task1
  runTask day1Task2

day1Task1 :: Task 1 Int
day1Task1 = pure $ computeTask1 input

day1Task2 :: Task 2 Int
day1Task2 = pure $ computeTask2 input

computeTask1 :: [String] -> Int
computeTask1 = foldl' (+) 0 . fmap computeLine

computeLine :: String -> Int
computeLine [] = 0
computeLine xs = read . (\s -> if null s then "0" else s) . (\(f, l) -> maybeToList f <> maybeToList l) $ foldl' go (Nothing, Nothing) xs
  where
    go :: (Maybe Char, Maybe Char) -> Char -> (Maybe Char, Maybe Char)
    go fl c
      | isDigit c = case fl of
          (Nothing, Nothing) -> (Just c, Just c)
          (Nothing, l) -> (Just c, l)
          (f, _) -> (f, Just c)
      | otherwise = fl

computeTask2 :: [String] -> Int
computeTask2 [] = 0
computeTask2 xs = foldl' (+) 0 $ read . uncurry (<>) . bimap maybeToList maybeToList . go (Nothing, Nothing) <$> xs
  where
    go :: (Maybe Char, Maybe Char) -> String -> (Maybe Char, Maybe Char)
    go fl "" = fl
    go fl s
      | isDigit (head s) = go (firstLast fl (head s)) (tail s)
      | couldBeAWord s = case parseDigit s of
          Just (i, rest) -> go (firstLast fl i) rest
          Nothing -> go fl (tail s)
      | otherwise = go fl (tail s)
    firstLast :: (Maybe Char, Maybe Char) -> Char -> (Maybe Char, Maybe Char)
    firstLast (Nothing, Nothing) c = (Just c, Just c)
    firstLast (Nothing, l) c = (Just c, l)
    firstLast (f, _) c = (f, Just c)

couldBeAWord :: String -> Bool
couldBeAWord [] = False
couldBeAWord (x : _) = x == 'o' || x == 't' || x == 'f' || x == 's' || x == 'e' || x == 'n'

parseDigit :: String -> Maybe (Char, String)
parseDigit s = asum $ go <$> digits
  where
    go (digit, i) =
      if digit `isPrefixOf` s
        then -- Instead of returning `drop (length digit) s`, we return `tail s` to catch edge cases like `eightwo`.
          Just (i, tail s)
        else Nothing

digits :: [(String, Char)]
digits =
  [ ("one", '1'),
    ("two", '2'),
    ("three", '3'),
    ("four", '4'),
    ("five", '5'),
    ("six", '6'),
    ("seven", '7'),
    ("eight", '8'),
    ("nine", '9')
  ]

input :: [String]
input = lines $ trim day1Input
