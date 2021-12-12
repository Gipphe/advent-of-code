module Y2021.Day6
    ( someDay6
    , day6Task1
    , day6Task2
    , computeTask1
    , computeTask2
    , input
    ) where

import Control.Arrow ((&&&))
import Data.List (group, sort)

import Day (SomeDay(..), Task, runTask)
import Util (split)
import Y2021.Input (day6Input)

someDay6 :: SomeDay
someDay6 = SomeDay @6 do
    runTask day6Task1
    runTask day6Task2

day6Task1 :: Task 1 Int
day6Task1 = pure $ computeTask1 input

day6Task2 :: Task 2 Int
day6Task2 = pure $ computeTask2 input

computeTask1 :: [FishGroup] -> Int
computeTask1 = sum . fmap numFish . iterateFish 80

computeTask2 :: [FishGroup] -> Int
computeTask2 = sum . fmap numFish . iterateFish 256

iterateFish :: Int -> [FishGroup] -> [FishGroup]
iterateFish 0 fs = fs
iterateFish n fs = iterateFish (n - 1) $ genFish fs

genFish :: [FishGroup] -> [FishGroup]
genFish fs = if new == 0
    then rest
    else FishGroup 6 new : FishGroup 8 new : rest
  where
    (z, nz) = span ((<= 0) . lifetime) $ sort fs
    rest    = (\(FishGroup l n) -> FishGroup (l - 1) n) <$> nz
    new     = sum $ numFish <$> z

input :: [FishGroup]
input =
    fmap (uncurry FishGroup . (head &&& length))
        . group
        . sort
        . fmap read
        . split ','
        $ day6Input

data FishGroup = FishGroup
    { lifetime :: {-# UNPACK #-} !Int
    , numFish  :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Ord)
