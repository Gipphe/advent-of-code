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

day5Input :: String
day5Input = $(embedStringFile "input/2022/day5.txt")

day5ExampleInput :: String
day5ExampleInput = $(embedStringFile "input/2022/day5.example.txt")

day6Input :: String
day6Input = $(embedStringFile "input/2022/day6.txt")

day7Input :: String
day7Input = $(embedStringFile "input/2022/day7.txt")

day7ExampleInput :: String
day7ExampleInput = $(embedStringFile "input/2022/day7.example.txt")

day8Input :: String
day8Input = $(embedStringFile "input/2022/day8.txt")

day8ExampleInput :: String
day8ExampleInput = $(embedStringFile "input/2022/day8.example.txt")
