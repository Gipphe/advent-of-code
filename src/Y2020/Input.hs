{-# LANGUAGE TemplateHaskell #-}

module Y2020.Input
    ( day1Input
    , day2Input
    , day3Input
    , day4Input
    , day5Input
    , day6Input
    , day7Input
    , day8Input
    , day9Input
    , day10Input
    , day11Input
    , day12Input
    , day13Input
    , day14Input
    , day15Input
    , day16Input
    , day17Input
    , day18Input
    , day19Input
    ) where

import Data.FileEmbed (embedStringFile)

day1Input :: String
day1Input = $(embedStringFile "input/2020/day1.txt")

day2Input :: String
day2Input = $(embedStringFile "input/2020/day2.txt")

day3Input :: String
day3Input = $(embedStringFile "input/2020/day3.txt")

day4Input :: String
day4Input = $(embedStringFile "input/2020/day4.txt")

day5Input :: String
day5Input = $(embedStringFile "input/2020/day5.txt")

day6Input :: String
day6Input = $(embedStringFile "input/2020/day6.txt")

day7Input :: String
day7Input = $(embedStringFile "input/2020/day7.txt")

day8Input :: String
day8Input = $(embedStringFile "input/2020/day8.txt")

day9Input :: String
day9Input = $(embedStringFile "input/2020/day9.txt")

day10Input :: String
day10Input = $(embedStringFile "input/2020/day10.txt")

day11Input :: String
day11Input = $(embedStringFile "input/2020/day11.txt")

day12Input :: String
day12Input = $(embedStringFile "input/2020/day12.txt")

day13Input :: String
day13Input = $(embedStringFile "input/2020/day13.txt")

day14Input :: String
day14Input = $(embedStringFile "input/2020/day14.txt")

day15Input :: String
day15Input = $(embedStringFile "input/2020/day15.txt")

day16Input :: String
day16Input = $(embedStringFile "input/2020/day16.txt")

day17Input :: String
day17Input = $(embedStringFile "input/2020/day17.txt")

day18Input :: String
day18Input = $(embedStringFile "input/2020/day18.txt")

day19Input :: String
day19Input = $(embedStringFile "input/2020/day19.txt")
