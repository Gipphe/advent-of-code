module Y2021.Day5 where

import qualified Data.Map.Strict as M
import Data.Void (Void)
import Text.Megaparsec (errorBundlePretty, parse, sepEndBy1, some)
import Text.Megaparsec.Char (char, digitChar, string)

import Day (SomeDay(..), Task, runTask)
import Y2021.Input (day5Input)

someDay5 :: SomeDay
someDay5 = SomeDay @5 do
    runTask day5Task1
    runTask day5Task2

day5Task1 :: Task 1 Int
day5Task1 = pure $ computeTask1 input

day5Task2 :: Task 2 Int
day5Task2 = pure $ computeTask2 input

computeTask1 :: Field -> Int
computeTask1 = doWerk enumerateLine

computeTask2 :: Field -> Int
computeTask2 = doWerk enumerateLineAndDiags

doWerk :: (Line -> [Point]) -> Field -> Int
doWerk f =
    M.size
        . M.filter (>= 2)
        . M.fromListWith (+)
        . mconcat
        . fmap (fmap (, 1 :: Int) . f)
        . unField

input :: Field
input = parseInput day5Input

data Point = Point
    { cx :: {-# UNPACK #-} !Int
    , cy :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Ord)

instance Show Point where
    show (Point x y) = show x <> "," <> show y

data Line = Line
    { start :: {-# UNPACK #-} !Point
    , end   :: {-# UNPACK #-} !Point
    }
    deriving (Eq, Ord)

instance Show Line where
    show (Line start end) = show start <> " -> " <> show end

enumerateLine :: Line -> [Point]
enumerateLine (Line (Point x1 y1) (Point x2 y2))
    | x1 == x2  = Point x1 <$> [min y1 y2 .. max y1 y2]
    | y1 == y2  = flip Point y1 <$> [min x1 x2 .. max x1 x2]
    | otherwise = []

enumerateLineAndDiags :: Line -> [Point]
enumerateLineAndDiags (Line (Point x1 y1) (Point x2 y2))
    | x1 == x2  = Point x1 <$> [min y1 y2 .. max y1 y2]
    | y1 == y2  = flip Point y1 <$> [min x1 x2 .. max x1 x2]
    | otherwise = uncurry Point <$> zip xr yr
  where
    xr = (if x1 > x2 then reverse else id) [min x1 x2 .. max x1 x2]
    yr = (if y1 > y2 then reverse else id) [min y1 y2 .. max y1 y2]

newtype Field = Field { unField :: [Line] }
    deriving (Eq, Ord)

parseInput :: String -> Field
parseInput = either (error . errorBundlePretty @_ @Void) id
    . parse fieldP "field"
  where
    fieldP  = Field <$> sepEndBy1 lineP (char '\n')
    lineP   = Line <$> coordP <* string " -> " <*> coordP
    coordP  = Point <$> numberP <* char ',' <*> numberP
    numberP = read <$> some digitChar
