{-# LANGUAGE TemplateHaskell #-}

module Y2023.Input (
  day1Input,
  day1ExampleInput,
  day1Example2Input,
  day2Input,
  day2ExampleInput,
)
where

import Data.FileEmbed (embedStringFile)

day1Input :: String
day1Input = $(embedStringFile "input/2023/day1.txt")

day1ExampleInput :: String
day1ExampleInput = $(embedStringFile "input/2023/day1.example.txt")

day1Example2Input :: String
day1Example2Input = $(embedStringFile "input/2023/day1.example2.txt")

day2Input :: String
day2Input = $(embedStringFile "input/2023/day2.txt")

day2ExampleInput :: String
day2ExampleInput = $(embedStringFile "input/2023/day2.example.txt")
