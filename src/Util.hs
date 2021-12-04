module Util
    ( splitOnDoubleNewline
    , trim
    , toSnd
    , split
    , splitOn
    , uncurry3
    , uncurry4
    , chainl1
    ) where

import Control.Applicative ((<**>), Alternative(..))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)


splitOnDoubleNewline :: String -> [String]
splitOnDoubleNewline = go []
  where
    go acc []                   = [reverse acc]
    go acc ('\n' : '\n' : rest) = reverse acc : go [] rest
    go acc (x           : rest) = go (x : acc) rest

splitOn :: Char -> String -> [String]
splitOn c = go []
  where
    go acc []                    = [reverse acc]
    go acc (c' : rest) | c == c' = reverse acc : go [] rest
    go acc (x : rest)            = go (x : acc) rest

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
{-# INLINE trim #-}

split :: Char -> String -> [String]
split _ "" = []
split c s  = cons $ case break (== c) s of
    (l, s') ->
        ( l
        , case s' of
            []      -> []
            _ : s'' -> split c s''
        )
    where cons ~(h, t) = h : t

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)
{-# INLINE toSnd #-}

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
{-# INLINE uncurry3 #-}

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d
{-# INLINE uncurry4 #-}

chainl1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = p <**> rst
    where rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id
{-# INLINE chainl1 #-}
