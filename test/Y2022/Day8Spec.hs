module Y2022.Day8Spec where

import Test.Tasty.HUnit
import Y2022.Day8

unit_Day8_computeTask1 :: Assertion
unit_Day8_computeTask1 = computeTask1 input @?= 1820

unit_Day8_computeTask2 :: Assertion
unit_Day8_computeTask2 = computeTask2 input @?= 385112
