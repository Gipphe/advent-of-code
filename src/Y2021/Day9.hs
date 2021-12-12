module Y2021.Day9
    ( someDay9
    , day9Task1
    , day9Task2
    , computeTask1
    , computeTask2
    , input
    ) where

import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down(Down))
import Data.Vector (Vector)

import Day (SomeDay(..), Task, runTask)
import qualified Grid as G
import Grid (Grid)
import Types (Point(..), arePointsAdjacent)
import Y2021.Input (day9Input)

someDay9 :: SomeDay
someDay9 = SomeDay @9 do
    runTask day9Task1
    runTask day9Task2

day9Task1 :: Task 1 Int
day9Task1 = pure $ computeTask1 input

day9Task2 :: Task 2 Int
day9Task2 = pure $ computeTask2 input

computeTask1 :: [[Int]] -> Int
computeTask1 g =
    sum
        $   (\y -> sum $ (+ 1) <$> mapMaybe
                (\x -> lookAround y x grid)
                [0 .. (columns - 1)]
            )
        <$> [0 .. (rows - 1)]
  where
    (rows, columns) = G.size grid
    grid            = G.fromList g

lookAround :: Int -> Int -> IntGrid -> Maybe Int
lookAround y x g
    | middle < up && middle < right && middle < down && middle < left
    = Just $! middle
    | otherwise
    = Nothing
  where
    up     = fromMaybe maxBound $ G.lookup (y - 1) x g
    right  = fromMaybe maxBound $ G.lookup y (x + 1) g
    down   = fromMaybe maxBound $ G.lookup (y + 1) x g
    left   = fromMaybe maxBound $ G.lookup y (x - 1) g
    middle = fromMaybe maxBound $ G.lookup y x g

computeTask2 :: [[Int]] -> Int
computeTask2 =
    product
        . take 3
        . sortOn Down
        . fmap rpitSize
        . combineRPits
        . foldMap
              ( fmap (singletonRPit . fst)
              . filter ((< 9) . snd)
              . uncurry addPoint
              )
        . zip [0 ..]

addPoint :: Int -> [Int] -> [(Point, Int)]
addPoint py = fmap (\(px, i) -> (Point { px, py }, i)) . zip [0 ..]


--------
-- * Pit
--------

data Pit
    = RPit !Point !Pit
    | SPit !Point

instance Show Pit where
    show (SPit p  ) = "SPit " <> show p
    show (RPit p r) = "RPit " <> show p <> "\n" <> show r

instance Semigroup Pit where
    (<>) = mergeRPits

rpitSize :: Pit -> Int
rpitSize = go
  where
    go (SPit _  ) = 1
    go (RPit _ r) = 1 + go r

rpitPoints :: Pit -> [Point]
rpitPoints (SPit p  ) = [p]
rpitPoints (RPit p r) = p : rpitPoints r

singletonRPit :: Point -> Pit
singletonRPit = SPit

mergeRPits :: Pit -> Pit -> Pit
mergeRPits (SPit p    ) b = RPit p b
mergeRPits (RPit p1 r1) b = RPit p1 $! mergeRPits r1 b

combineRPits :: [Pit] -> [Pit]
combineRPits [] = []
combineRPits (pit : pits)
    | changed   = combineRPits (pit' : rest')
    | otherwise = pit' : combineRPits rest'
  where
    (pit', changed, rest') = foldl'
        (\(p, c, rest) o ->
            if isAdjacent p o then (p <> o, True, rest) else (p, c, o : rest)
        )
        (pit, False, [])
        pits

isAdjacent :: Pit -> Pit -> Bool
isAdjacent p1' p2' = or $ arePointsAdjacent <$> p1 <*> p2
  where
    p1 = rpitPoints p1'
    p2 = rpitPoints p2'


---------
-- * Misc
---------

input :: [[Int]]
input = fmap (fmap digitToInt) . filter (not . null) . lines $ day9Input

type IntGrid = Grid Vector Int
