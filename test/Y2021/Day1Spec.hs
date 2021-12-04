module Y2021.Day1Spec where

import Test.Tasty.HUnit
import Y2021.Day1

unit_Day1_computeTask1 :: Assertion
unit_Day1_computeTask1 = computeTask1 input @?= 1532

unit_Day1_computeTask2 :: Assertion
unit_Day1_computeTask2 = computeTask2 input @?= 1571
