module Y2022.Day1Spec where

import Test.Tasty.HUnit
import Y2022.Day1

unit_Day1_computeTask1 :: Assertion
unit_Day1_computeTask1 = computeTask1 input @?= 67027

unit_Day1_computeTask2 :: Assertion
unit_Day1_computeTask2 = computeTask2 input @?= 197291
