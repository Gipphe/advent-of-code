{-# LANGUAGE TemplateHaskell #-}

module Y2022.Input
    ( day1Input
    ) where

import Data.FileEmbed (embedStringFile)

day1Input :: String
day1Input = $(embedStringFile "input/2022/day1.txt")
