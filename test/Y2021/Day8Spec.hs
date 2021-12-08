module Y2021.Day8Spec where

import Test.Tasty.HUnit
import Y2021.Day8

unit_Day8_computeTask1 :: Assertion
unit_Day8_computeTask1 = computeTask1 input @?= 278

unit_Day8_computeTask2 :: Assertion
unit_Day8_computeTask2 = computeTask2 input @?= 986179
