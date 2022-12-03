module Y2021.Day12
    ( someDay12
    , day12Task1
    , computeTask1
    , input
    ) where

import Data.Bifunctor (bimap)
import Data.Char (isUpper)
import Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Set (Set)

import Data.Foldable (foldl')
import Day (SomeDay(..), Task, runTask)
import Y2021.Input (day12Input)

someDay12 :: SomeDay
someDay12 = SomeDay @12 do
    runTask day12Task1

day12Task1 :: Task 1 Int
day12Task1 = pure $ computeTask1 input

computeTask1 :: Map Cave [Cave] -> Int
computeTask1 m = findEnd mempty m $ m ! Start

findEnd :: Set Cave -> Map Cave [Cave] -> [Cave] -> Int
findEnd _ _ [] = 0
findEnd visited m caves =
    ends + findEnd (visited <> S.fromList smallCaves) m withoutSmallVisitedCaves
  where
    upcomingCaves =
        filter (/= Start) $ foldl' (\acc c -> (m ! c) <> acc) [] caves
    (ends, notEnd)           = countNothings upcomingCaves
    withoutSmallVisitedCaves = filter (`S.notMember` visited) notEnd
    smallCaves               = filter isSmallCave withoutSmallVisitedCaves
    countNothings            = foldl'
        (\(n, acc) -> \case
            End -> (n + 1, acc)
            x   -> (n, x : acc)
        )
        (0 :: Int, [])


data Cave
    = Start
    | SmallCave String
    | BigCave String
    | End
    deriving (Eq, Ord)

isSmallCave :: Cave -> Bool
isSmallCave (SmallCave _) = True
isSmallCave _             = False

input :: Map Cave [Cave]
input = parseRelations day12Input

parseRelations :: String -> Map Cave [Cave]
parseRelations = foldl' go mempty . fmap parseRelation . lines
  where
    go m (c1, c2) = M.insertWith (<>) c1 [c2] $ M.insertWith (<>) c2 [c1] m

parseRelation :: String -> (Cave, Cave)
parseRelation = bimap parseCave parseCave . span (/= '-')

parseCave :: String -> Cave
parseCave "start" = Start
parseCave "end"   = End
parseCave s
    | all isUpper s = BigCave s
    | otherwise     = SmallCave s
