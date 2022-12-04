{-# LANGUAGE TemplateHaskell #-}

module Y2022.Input where

import Data.FileEmbed (embedStringFile)

day1Input :: String
day1Input = $(embedStringFile "input/2022/day1.txt")

day2Input :: String
day2Input = $(embedStringFile "input/2022/day2.txt")

day3Input :: String
day3Input = $(embedStringFile "input/2022/day3.txt")

day3ExampleInput :: String
day3ExampleInput = $(embedStringFile "input/2022/day3.example.txt")

day4Input :: String
day4Input = $(embedStringFile "input/2022/day4.txt")

day4ExampleInput :: String
day4ExampleInput = $(embedStringFile "input/2022/day4.example.txt")
