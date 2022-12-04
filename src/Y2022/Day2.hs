module Y2022.Day2
    ( someDay2
    , computeTask1
    , computeTask2
    , input
    ) where

import Day (Day, SomeDay(..), Task, runTask)
import Y2022.Input (day2Input)

input :: [String]
input = lines day2Input

someDay2 :: SomeDay
someDay2 = SomeDay day2

day2 :: Day 2 ()
day2 = do
    runTask day2Task1
    runTask day2Task2

day2Task1 :: Task 1 Int
day2Task1 = pure $ computeTask1 input

day2Task2 :: Task 2 Int
day2Task2 = pure $ computeTask2 input

computeTask1 :: [String] -> Int
computeTask1 = sum . fmap (sumResult . runRound . readRound)

computeTask2 :: [String] -> Int
computeTask2 = sum . fmap (sumResult . runSuggestion . readSuggestion)

data Round = Round Hand Hand

readRound :: String -> Round
readRound [their, ' ', mine] = Round (readHand their) (readHand mine)
readRound x                  = error $ "Unable to read match: " <> x

runRound :: Round -> Result
runRound (Round their mine) = Result their mine (myOutcome mine their)

data Result = Result Hand Hand Outcome

sumResult :: Result -> Int
sumResult (Result _ mine result) = handPoints mine + outcomePoints result

data Hand = Rock | Paper | Scissors
    deriving (Eq)

getBeatingHand :: Hand -> Hand
getBeatingHand Rock     = Paper
getBeatingHand Paper    = Scissors
getBeatingHand Scissors = Rock

getLosingHand :: Hand -> Hand
getLosingHand Rock     = Scissors
getLosingHand Paper    = Rock
getLosingHand Scissors = Paper

handPoints :: Hand -> Int
handPoints = \case
    Rock     -> 1
    Paper    -> 2
    Scissors -> 3

readHand :: Char -> Hand
readHand = \case
    'A' -> Rock
    'X' -> Rock
    'B' -> Paper
    'Y' -> Paper
    'C' -> Scissors
    'Z' -> Scissors
    x   -> error $ "Invalid hand: " <> show x

data Outcome = Win | Loss | Draw

myOutcome :: Hand -> Hand -> Outcome
myOutcome Paper    Rock     = Win
myOutcome Rock     Scissors = Win
myOutcome Scissors Paper    = Win
myOutcome x        y        = if x == y then Draw else Loss

readOutcome :: Char -> Outcome
readOutcome = \case
    'X' -> Loss
    'Y' -> Draw
    'Z' -> Win
    x   -> error $ "Invalid outcome: " <> show x

outcomePoints :: Outcome -> Int
outcomePoints Win  = 6
outcomePoints Draw = 3
outcomePoints Loss = 0

data Suggestion = Suggestion Hand Outcome

readSuggestion :: String -> Suggestion
readSuggestion [their, ' ', outcome] =
    Suggestion (readHand their) (readOutcome outcome)
readSuggestion x = error $ "Unknown suggestion: " <> x

runSuggestion :: Suggestion -> Result
runSuggestion (Suggestion x Win ) = Result x (getBeatingHand x) Win
runSuggestion (Suggestion x Loss) = Result x (getLosingHand x) Loss
runSuggestion (Suggestion x Draw) = Result x x Draw
