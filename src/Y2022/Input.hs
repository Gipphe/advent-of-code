{-# LANGUAGE TemplateHaskell #-}

module Y2022.Input where

import Data.FileEmbed (embedStringFile)

day1Input :: String
day1Input = $(embedStringFile "input/2022/day1.txt")

day2Input :: String
day2Input = $(embedStringFile "input/2022/day2.txt")
