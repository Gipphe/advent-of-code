module Y2021.Day11
    ( someDay11
    , day11Task1
    , computeTask1
    , input
    ) where

import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Day (SomeDay(..), Task, runTask)
import Types (Point(..))
import Y2021.Input (day11Input)

someDay11 :: SomeDay
someDay11 = SomeDay @11 do
    runTask day11Task1
    runTask day11Task2

day11Task1 :: Task 1 Int
day11Task1 = pure $ computeTask1 input

day11Task2 :: Task 2 Int
day11Task2 = pure $ computeTask2 input

computeTask1 :: OctoGrid -> Int
computeTask1 g = fst $ foldl' go (0, g) [1 :: Int .. 100]
  where
    go (flashes, m) _ = let (n, m') = iterateOctos m in (flashes + n, m')

computeTask2 :: OctoGrid -> Int
computeTask2 = go 1
  where
    go :: Int -> OctoGrid -> Int
    go rounds g =
        let (!n, !g') = iterateOctos g
        in if n /= 100 then go (rounds + 1) g' else rounds

iterateOctos :: OctoGrid -> (Int, OctoGrid)
iterateOctos = resetOctos . flashOctos . incOctos

flashOctos :: OctoGrid -> OctoGrid
flashOctos g = flash g . filterFlashing $ M.toList g
  where
    filterFlashing = fmap fst . filter ((== 10) . octoLevel . snd)
    flash :: OctoGrid -> [Point] -> OctoGrid
    flash g' [] = g'
    flash g' ps = foldl' go g' ps
    go m i = flash incedNeighs flashingNeighs
      where
        neigh       = filter (`M.member` m) $ neighbours i
        incedNeighs = foldr (M.adjust incOcto) m neigh
        flashingNeighs =
            filterFlashing . M.toList . M.restrictKeys incedNeighs $ S.fromList
                neigh

resetOctos :: OctoGrid -> (Int, OctoGrid)
resetOctos m = M.foldlWithKey'
    (\(!n, !m') p o ->
        if octoFlashed o then (n + 1, M.insert p (Octo 0) m') else (n, m')
    )
    (0, m)
    m

neighbours :: Point -> [Point]
neighbours p =
    [ Point { py = py p + r', px = px p + c' }
    | r' <- [-1, 0, 1]
    , c' <- [-1, 0, 1]
    , not (r' == 0 && c' == 0)
    ]

incOctos :: OctoGrid -> OctoGrid
incOctos = M.map incOcto

input :: OctoGrid
input =
    M.fromList
        . foldMap
              (\(py, os) ->
                  fmap (\(px, o) -> (Point { py, px }, o))
                      .   zip [0 ..]
                      $   mkOcto
                      <$> os
              )
        . zip [0 ..]
        . filter (not . null)
        . lines
        $ day11Input

newtype Octo = Octo
    { octoLevel :: Int
    }

mkOcto :: Char -> Octo
mkOcto = Octo . max 0 . min 9 . digitToInt

incOcto :: Octo -> Octo
incOcto o = o { octoLevel = octoLevel o + 1 }

octoFlashed :: Octo -> Bool
octoFlashed = (> 9) . octoLevel

type OctoGrid = Map Point Octo
