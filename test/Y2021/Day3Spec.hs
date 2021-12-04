module Y2021.Day3Spec where

import Test.Tasty.HUnit
import Y2021.Day3

unit_Day3_computeTask1 :: Assertion
unit_Day3_computeTask1 = computeTask1 input @?= 741950

unit_Day3_computeTask2 :: Assertion
unit_Day3_computeTask2 = computeTask2 input @?= 903810
