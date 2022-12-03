module Y2022.Day3
    ( someDay3
    , day3Task1
    , computeTask1
    , input
    ) where

import Data.Char (isLower, ord)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Day (Day, SomeDay(..), Task, runTask)
import Y2022.Input (day3Input)

input :: [String]
input = lines day3Input

someDay3 :: SomeDay
someDay3 = SomeDay day3

day3 :: Day 3 ()
day3 = do
    runTask day3Task1
    runTask day3Task2

day3Task1 :: Task 1 Int
day3Task1 = pure $ computeTask1 input

day3Task2 :: Task 2 Int
day3Task2 = pure $ computeTask2 input

computeTask1 :: [String] -> Int
computeTask1 = sum . fmap (getPriority . searchRucksack M.empty . readRucksack)

computeTask2 :: [String] -> Int
computeTask2 = sum . fmap (getPriority . findGroupItemType) . splitIntoGroups

data Rucksack = Rucksack String String

readRucksack :: String -> Rucksack
readRucksack s =
    Rucksack (take (length s `div` 2) s) (drop (length s `div` 2) s)

data Side = L | R
    deriving (Eq, Show)

searchRucksack :: Map Char Side -> Rucksack -> Char
searchRucksack m (Rucksack (x : l) r) = if M.lookup x m == Just R
    then x
    else searchRucksack (M.insert x L m) (Rucksack l r)
searchRucksack m (Rucksack [] (x : r)) = if M.lookup x m == Just L
    then x
    else searchRucksack (M.insert x R m) (Rucksack [] r)
searchRucksack m _ = error $ "Unable to find error: " <> show m

getPriority :: Char -> Int
getPriority x
    | isLower x = ord x - ord 'a' + 1
    | otherwise = ord x - ord 'A' + 27

data Group = Group String String String

splitIntoGroups :: [String] -> [Group]
splitIntoGroups (x : y : z : rest) = Group x y z : splitIntoGroups rest
splitIntoGroups _                  = []

findGroupItemType :: Group -> Char
findGroupItemType (Group xs ys zs) =
    fst
        . fromJust
        . find (\(_, i) -> i >= 3)
        . M.toList
        $ M.unionWith (+) (foldRucksack zs)
        $ M.unionWith (+) (foldRucksack xs) (foldRucksack ys)
  where
    foldRucksack xs' = foldr (\x m -> M.insert x (1 :: Int) m) M.empty xs'
