module Y2021.Day4 where

import Data.Bifunctor (Bifunctor(second), bimap)
import Data.List (transpose)
import Data.Void (Void)
import Text.Megaparsec
    (Parsec, errorBundlePretty, many, parse, sepBy1, some, try)
import Text.Megaparsec.Char (char, digitChar)

import Day (Day, SomeDay(..), Task, runTask)
import Util (splitOnDoubleNewline, trim)
import Y2021.Input (day4Input)

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

computeTask1 :: ([Int], [Board]) -> Int
computeTask1 =
    uncurry (*) . second getUnMarkedSum . head . uncurry iterateBoards

computeTask2 :: ([Int], [Board]) -> Int
computeTask2 =
    uncurry (*) . second getUnMarkedSum . last . uncurry iterateBoards

iterateBoards :: [Int] -> [Board] -> [(Int, Board)]
iterateBoards _        []     = []
iterateBoards []       _      = []
iterateBoards (x : xs) boards = fmap (x, ) winners <> iterateBoards xs losers
  where
    boards'           = markBoards x boards
    (losers, winners) = foldr
        (\b (l, w) -> if hasWon b then (l, b : w) else (b : l, w))
        ([], [])
        boards'

input :: ([Int], [Board])
input = parseInput day4Input

newtype Board = Board { unBoard :: [[(Int, Bool)]] }

instance Show Board where
    show (Board rows) = unlines $ fmap (unwords . fmap showCell) rows

showCell :: (Int, Bool) -> String
showCell (x, marked)
    | marked && x < 10     = "> " <> show x <> "<"
    | not marked && x < 10 = "  " <> show x <> " "
    | marked               = ">" <> show x <> "<"
    | otherwise            = " " <> show x <> " "

markBoards :: Int -> [Board] -> [Board]
markBoards n = fmap (markBoard n)

getUnMarkedSum :: Board -> Int
getUnMarkedSum = sum . mconcat . fmap (fmap fst . filter (not . snd)) . unBoard

markBoard :: Int -> Board -> Board
markBoard x =
    Board
        . fmap (fmap (\(n, m) -> if x == n then (n, True) else (n, m)))
        . unBoard

hasWon :: Board -> Bool
hasWon (Board board) = any (all snd) board || any (all snd) (transpose board)

parseInput :: String -> ([Int], [Board])
parseInput = bimap parseNumberLine parseBoards . separate . lines
  where
    separate (x : "" : xs) = (x, unlines xs)
    separate _             = error "parseInput: empty list"

parseNumberLine :: String -> [Int]
parseNumberLine = either (error . errorBundlePretty) id
    . parse parser "numbers"
    where parser = sepBy1 numberP (char ',')

parseBoards :: String -> [Board]
parseBoards =
    fmap ((either (error . errorBundlePretty) id . parse boardP "board") . trim)
        . splitOnDoubleNewline
  where
    boardP = Board <$> sepBy1 rowP (char '\n')
    rowP =
        many (char ' ')
            *> (fmap (, False) <$> sepBy1 numberP (try (some (char ' '))))

numberP :: Parser Int
numberP = read <$> some digitChar

type Parser = Parsec Void String
