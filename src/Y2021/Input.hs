{-# LANGUAGE TemplateHaskell #-}

module Y2021.Input
    ( day1Input
    , day2Input
    , day3Input
    , day4Input
    , day4ExampleInput
    , day5Input
    , day5ExampleInput
    , day6Input
    , day7Input
    , day8Input
    , day8ExampleInput
    , day9Input
    , day10Input
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

day5Input :: String
day5Input = $(embedStringFile "input/2021/day5.txt")

day5ExampleInput :: String
day5ExampleInput = $(embedStringFile "input/2021/day5.example.txt")

day6Input :: String
day6Input = $(embedStringFile "input/2021/day6.txt")

day7Input :: String
day7Input = $(embedStringFile "input/2021/day7.txt")

day8Input :: String
day8Input = $(embedStringFile "input/2021/day8.txt")

day8ExampleInput :: String
day8ExampleInput = $(embedStringFile "input/2021/day8.example.txt")

day9Input :: String
day9Input = $(embedStringFile "input/2021/day9.txt")

day10Input :: String
day10Input = $(embedStringFile "input/2021/day10.txt")
