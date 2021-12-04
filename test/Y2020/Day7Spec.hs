module Y2020.Day7Spec where

import Test.Tasty.HUnit
import Y2020.Day7

unit_Day7_computeTask1 :: Assertion
unit_Day7_computeTask1 = computeTask1 parsedInput @?= 144

unit_Day7_computeTask2 :: Assertion
unit_Day7_computeTask2 = computeTask2 parsedInput @?= 5956
