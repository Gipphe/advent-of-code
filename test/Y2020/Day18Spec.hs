module Y2020.Day18Spec where

import Test.Tasty.HUnit
import Y2020.Day18

unit_Day18_computeTask1 :: Assertion
unit_Day18_computeTask1 = computeTask1 parsedInput @?= (800602729153 :: Int)

unit_Day18_computeTask2 :: Assertion
unit_Day18_computeTask2 = computeTask2 parsedInput @?= (92173009047076 :: Int)
