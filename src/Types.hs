module Types where

import Data.List (intercalate)

data Point = Point
    { py :: {-# UNPACK #-} !Int
    , px :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Ord, Show)

arePointsAdjacent :: Point -> Point -> Bool
arePointsAdjacent (Point y1 x1) (Point y2 x2) =
    absDist y1 y2 + absDist x1 x2 == 1
    where absDist x y = abs (x - y)

newtype NewlineList a = NewlineList { getNewlineList :: [a] }
    deriving Semigroup via [a]

instance Show a => Show (NewlineList a) where
    show (NewlineList xs) =
        (<> "\n") . ("\n" <>) . intercalate "\n" $ ("> " <>) . show <$> xs
    {-# INLINABLE show #-}
