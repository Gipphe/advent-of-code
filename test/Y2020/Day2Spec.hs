module Y2020.Day2Spec where

import Test.Tasty.HUnit
import Y2020.Day2

unit_Day2_computeTask1 :: Assertion
unit_Day2_computeTask1 = computeTask1 parsedInput @?= 600

unit_Day2_computeTask2 :: Assertion
unit_Day2_computeTask2 = computeTask2 parsedInput @?= 245
