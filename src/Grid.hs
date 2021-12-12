{-# LANGUAGE UndecidableInstances #-}

module Grid
    ( Grid
    , getGrid
    , lookup
    , fromList
    , size
    , map
    ) where

import qualified Data.Vector.Generic as V
import Data.Vector.Generic ((!?), Vector)

import Data.Foldable (foldl')
import Prelude hiding (lookup, map)

newtype Grid v a = Grid { getGrid :: v (v a) }

instance (Griddable v a, Show a) => Show (Grid v a) where
    show (Grid g) = unlines $ mconcat . fmap show . V.toList <$> V.toList g

type Griddable v a = (Vector v a, Vector v (v a))

lookup :: (Griddable v a) => Int -> Int -> Grid v a -> Maybe a
lookup y x (Grid grid) = (!? x) =<< (grid !? y)
{-# INLINABLE lookup #-}

fromList :: (Griddable v a) => [[a]] -> Grid v a
fromList = Grid . V.fromList . fmap V.fromList . fst . foldl'
    limit
    ([], maxBound)
  where
    limit (acc, len) xs
        | xlen < len = (xs : fmap (take xlen) acc, xlen)
        | otherwise  = (take len xs : acc, len)
        where xlen = length xs
{-# INLINABLE fromList #-}

size :: (Griddable v a) => Grid v a -> (Int, Int)
size (Grid g) = (V.length g, V.length $ V.head g)
{-# INLINABLE size #-}

map :: (Griddable v a, Griddable v b) => (a -> b) -> Grid v a -> Grid v b
map f = Grid . V.map (V.map f) . getGrid
{-# INLINABLE map #-}
