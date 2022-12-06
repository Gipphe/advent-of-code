module Y2022.Day5
    ( someDay5
    , computeTask1
    , computeTask2
    , input
    ) where

import Data.Char (isAlpha)
import Data.List (transpose)
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Day (Day, SomeDay(..), Task, runTask)
import Util (headMaybe, split, splitOnDoubleNewline)
import Y2022.Input (day5Input)

input :: (Crates, Procedure)
input = parseInput day5Input

someDay5 :: SomeDay
someDay5 = SomeDay day5

day5 :: Day 5 ()
day5 = do
    runTask day5Task1
    runTask day5Task2

day5Task1 :: Task 1 String
day5Task1 = pure $ computeTask1 input

day5Task2 :: Task 2 String
day5Task2 = pure $ computeTask2 input

computeTask1 :: (Crates, Procedure) -> String
computeTask1 (!crates, !procedure)
    | null procedure
    = mapMaybe (headMaybe . fmap getCrate . getStack) $ V.toList $ V.drop
        1
        crates
    | otherwise
    = computeTask1 (stepSingle crates procedure)

computeTask2 :: (Crates, Procedure) -> String
computeTask2 (!crates, !procedure)
    | null procedure
    = mapMaybe (headMaybe . fmap getCrate . getStack) $ V.toList $ V.drop
        1
        crates
    | otherwise
    = computeTask2 (stepMulti crates procedure)

stepSingle :: Crates -> Procedure -> (Crates, Procedure)
stepSingle crates ((Move { from, to, n }) : rest) =
    case (newFromStack, newToStack) of
        (Just newFromStack', Just newToStack') ->
            let
                crates' =
                    crates V.// [(from, newFromStack'), (to, newToStack')]
            in
                if newN <= 0
                    then (crates', rest)
                    else (crates', (Move from to newN : rest))
        _ -> (crates, rest)
  where
    newN         = n - 1
    fromStack    = crates V.!? from
    crate        = peekStack =<< fromStack
    newFromStack = popStack <$> fromStack
    toStack      = crates V.!? to
    newToStack   = pushStack <$> crate <*> toStack
stepSingle crates [] = (crates, [])

stepMulti :: Crates -> Procedure -> (Crates, Procedure)
stepMulti crates ((Move { from, to, n }) : rest) =
    case (newFromStack, newToStack) of
        (Just newFromStack', Just newToStack') ->
            (crates V.// [(from, newFromStack'), (to, newToStack')], rest)
        _ -> (crates, rest)
  where
    fromStack    = crates V.!? from
    cratesToMove = take n . getStack <$> fromStack
    newFromStack = Stack . drop n . getStack <$> fromStack
    toStack      = crates V.!? to
    newToStack :: Maybe Stack
    newToStack =
        (\crates' -> Stack . (crates' <>) . getStack)
            <$> cratesToMove
            <*> toStack
stepMulti crates [] = (crates, [])

parseInput :: String -> (Crates, Procedure)
parseInput s = case splitOnDoubleNewline s of
    (crates : procedure : _) -> (parseCrates crates, parseProcedure procedure)
    unknown                  -> error $ "invalid input: " <> show unknown

type Crates = Vector Stack

parseCrates :: String -> Crates
parseCrates =
    (V.singleton (Stack []) <>)
        . V.fromList
        . fmap (parseStack . reverse)
        . filter (not . null)
        . fmap (filter isAlpha)
        . transpose
        . lines

newtype Stack = Stack { getStack :: [Crate] }
    deriving (Show) via [Crate]

parseStack :: String -> Stack
parseStack = Stack . reverse . fmap Crate

pushStack :: Crate -> Stack -> Stack
pushStack x = Stack . (x :) . getStack

peekStack :: Stack -> Maybe Crate
peekStack (Stack []) = Nothing
peekStack (Stack xs) = Just $ head xs

popStack :: Stack -> Stack
popStack (Stack []      ) = Stack []
popStack (Stack (_ : xs)) = Stack xs

newtype Crate = Crate { getCrate :: Char }
    deriving (Show) via Char

type Procedure = [Move]

parseProcedure :: String -> Procedure
parseProcedure = fmap parseMove . lines

data Move = Move
    { from :: Int
    , to   :: Int
    , n    :: Int
    }

instance Show Move where
    show (Move { from, to, n }) =
        "move " <> show n <> " from " <> show from <> " to " <> show to

parseMove :: String -> Move
parseMove s = case split ' ' s of
    ("move" : n : "from" : from : "to" : to : _) ->
        Move { from = (read from), to = (read to), n = (read n) }
    unknown -> error $ "invalid move: " <> show unknown
