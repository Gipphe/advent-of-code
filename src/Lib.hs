module Lib
    ( main
    ) where

import Data.List (intersperse)

import Data.Foldable (sequenceA_)
-- import Y2020
import Y2021

main :: IO ()
main = do
    sequenceA_ $ intersperse
        (putStrLn "\n---------------\n")
        [ --y2020,
         y2021]
