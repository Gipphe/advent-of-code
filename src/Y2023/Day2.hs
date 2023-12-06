module Y2023.Day2 (
  someDay2,
  day1Task1,
  day1Task2,
  computeTask1,
  computeTask2,
  input,
  gamesP,
)
where

import Control.Applicative (some)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Day (SomeDay (..), Task, runTask)
import Text.Megaparsec (Parsec, choice, parse, sepBy)
import Text.Megaparsec.Char (char, digitChar, newline, space, string)
import Util (traceShowLabelId, trim)
import Y2023.Input (day2ExampleInput)

someDay2 :: SomeDay
someDay2 = SomeDay @2 do
  runTask day1Task1
  runTask day1Task2

day1Task1 :: Task 1 Int
day1Task1 = pure $ computeTask1 input

day1Task2 :: Task 2 Int
day1Task2 = pure $ computeTask2 input

computeTask1 :: String -> Int
computeTask1 = sum . fmap (\(Game i _) -> i) . filter (isPossible limits) . parseInput

computeTask2 :: String -> Int
computeTask2 = sum . traceShowLabelId "powers: " . fmap (power . traceShowLabelId "current game: ") . parseInput

parseInput :: String -> [Game]
parseInput = either (error . show) id . parse gamesP ""

gamesP :: Parser [Game]
gamesP = sepBy gameP newline

gameP :: Parser Game
gameP = Game <$> (string "Game" *> space *> (read <$> some digitChar)) <* (char ':' *> space) <*> sepBy cubeSetP (char ';' *> space)

cubeSetP :: Parser CubeSet
cubeSetP = CubeSet <$> sepBy cubesP (char ',' *> space)

cubesP :: Parser Cubes
cubesP = Cubes <$> (read <$> some digitChar) <* space <*> colorP

colorP :: Parser Color
colorP =
  choice
    [ string "red" $> Red
    , string "green" $> Green
    , string "blue" $> Blue
    ]

type Parser = Parsec Void String

data Game = Game Int [CubeSet]
  deriving (Show)

newtype CubeSet = CubeSet [Cubes]
  deriving (Show)

data Cubes = Cubes Int Color
  deriving (Show)

data Color
  = Red
  | Green
  | Blue
  deriving (Eq, Ord, Show)

limits :: Color -> Int
limits = \case
  Red -> 12
  Green -> 13
  Blue -> 14

isPossible :: (Color -> Int) -> Game -> Bool
isPossible lims (Game _ ss) = all go ss
 where
  go (CubeSet cubes) = all go' cubes
  go' (Cubes n c) = n <= lims c

power :: Game -> Int
power (Game _ sets) = sum $ setPower <$> sets

setPower :: CubeSet -> Int
setPower (CubeSet ss) = product . traceShowLabelId "powers in set: " $ fromMaybe 0 . (`M.lookup` ms) <$> [Red, Green, Blue]
 where
  ms = traceShowLabelId "set powers: " $ foldl' go mempty ss
  go m (Cubes n c) = M.alter (Just . traceShowLabelId ("resulting " <> show c <> ": ") . max (traceShowLabelId ("possible " <> show c <> ": ") n) . traceShowLabelId ("current " <> show c <> ": ") . fromMaybe 0) c m

input :: String
input = trim day2ExampleInput
