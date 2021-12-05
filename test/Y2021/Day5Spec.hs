module Y2021.Day5Spec where

import Test.Tasty.HUnit
import Y2021.Day5

unit_Day5_computeTask1 :: Assertion
unit_Day5_computeTask1 = computeTask1 input @?= 5585

unit_Day5_computeTask2 :: Assertion
unit_Day5_computeTask2 = computeTask2 input @?= 17193
