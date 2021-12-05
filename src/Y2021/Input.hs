{-# LANGUAGE TemplateHaskell #-}

module Y2021.Input
    ( day1Input
    , day2Input
    , day3Input
    , day4Input
    , day4ExampleInput
    ) where

import Data.FileEmbed (embedStringFile)

day1Input :: String
day1Input = $(embedStringFile "input/2021/day1.txt")

day2Input :: String
day2Input = $(embedStringFile "input/2021/day2.txt")

day3Input :: String
day3Input = $(embedStringFile "input/2021/day3.txt")

day4Input :: String
day4Input = $(embedStringFile "input/2021/day4.txt")

day4ExampleInput :: String
day4ExampleInput = $(embedStringFile "input/2021/day4.example.txt")
