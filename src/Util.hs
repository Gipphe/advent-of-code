module Util
    ( splitOnDoubleNewline
    , trim
    , toSnd
    , split
    , splitOn
    , uncurry3
    , uncurry4
    , chainl1
    , digitsToDecimal
    , median
    , medianOfMedians
    , traceShowLabelId
    , headMaybe
    ) where

import Control.Applicative ((<**>), Alternative(..))
import Control.Arrow (second)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, sort)
import Debug.Trace (trace)


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

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (!a, !b, !c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (!a, !b, !c, !d) = f a b c d

chainl1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = p <**> rst
    where rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id

digitsToDecimal :: [Int] -> Int
digitsToDecimal = go 0 . reverse
  where
    go _          []       = 0
    go (n :: Int) (x : xs) = x * 10 ^ n + go (n + 1) xs

median :: Ord a => [a] -> a
median xs = sorted !! middlePoint
  where
    sorted      = sort xs
    middlePoint = length xs `div` 2

medianOfMedians :: [Int] -> Int
medianOfMedians [] = error "medianOfMedians: empty list"
medianOfMedians xs = xs' !! idx
    where (idx, xs') = select xs 0 (length xs) (length xs `div` 2)

select :: [Int] -> Int -> Int -> Int -> (Int, [Int])
select list left right n
    | left == right   = (left, list)
    | pivotIndex == n = (n, list)
    | n < pivotIndex  = select list' left (pivotIndex - 1) n
    | otherwise       = select list' (pivotIndex + 1) right n
  where
    (pivotIndex', pivoted) = pivot list left right
    (pivotIndex , list'  ) = partition pivoted left right pivotIndex' n

partition :: [Int] -> Int -> Int -> Int -> Int -> (Int, [Int])
partition list left right pivotIndex n
    | n < storeIndex    = (storeIndex, swappedAfter)
    | n <= storeIndexEq = (n, swappedAfter)
    | otherwise         = (storeIndexEq, swappedAfter)
  where
    pivotValue = list !! pivotIndex
    swapped    = swap list pivotIndex right
    moveSmaller i xs smallIdx
        | i >= right - 1 = (xs, smallIdx)
        | xs !! i < pivotValue = moveSmaller
            (i + 1)
            (swap xs smallIdx i)
            (smallIdx + 1)
        | otherwise = moveSmaller (i + 1) xs smallIdx
    (smallerMoved, storeIndex) = moveSmaller left swapped left
    moveEq i list' eqIdx
        | i >= right - 1 = (list', eqIdx)
        | list' !! i == pivotValue = moveEq
            (i + 1)
            (swap list' eqIdx i)
            (eqIdx + 1)
        | otherwise = moveEq (i + 1) list' eqIdx
    (allMoved, storeIndexEq) = moveEq storeIndex smallerMoved storeIndex
    swappedAfter             = swap allMoved right storeIndexEq

swap :: forall a . [a] -> Int -> Int -> [a]
swap []       _ _ = []
swap (x : xs) 0 j = y' : rest
  where
    (y', rest) = go xs j

    go :: [a] -> Int -> (a, [a])
    go []       _ = error "swap: index out of bounds"
    go (y : ys) 0 = (y, x : ys)
    go (y : ys) k = second (y :) $ go ys (k - 1)
swap (x : xs) i j = x : swap xs (i - 1) j

pivot :: [Int] -> Int -> Int -> (Int, [Int])
pivot list left right
    | right - left < 5 = partition5 list left right
    | otherwise = select
        (go list left)
        left
        (left + ((right - left) `div` 5))
        mid
  where
    mid = (right - left) `div` 10 + left + 1
    go l i
        | i < right = go (swap l median5 (left + ((i - left) `div` 5))) (i + 5)
        | otherwise = l
      where
        subRight     = min (i + 4) right
        (median5, _) = partition5 l i subRight

partition5 :: [Int] -> Int -> Int -> (Int, [Int])
partition5 xs left right = ((left + right) `div` 2, gone)
  where
    gone = go (left + 1) xs
    go i list
        | i <= right = go (i + 1) $ go2 i list
        | otherwise  = list
    go2 j list
        | j > left && xs !! (j - 1) > xs !! j = go2 (j - 1) (swap xs (j - 1) j)
        | otherwise = list

traceShowLabelId :: Show a => String -> a -> a
traceShowLabelId label a = trace (label <> show a) a

headMaybe :: [a] -> Maybe a
headMaybe xs
    | null xs   = Nothing
    | otherwise = Just (head xs)
