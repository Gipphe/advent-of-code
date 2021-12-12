module Y2021.Day9Spec where

import Test.Tasty.HUnit
import Y2021.Day9

unit_Day9_computeTask1 :: Assertion
unit_Day9_computeTask1 = computeTask1 input @?= 504

unit_Day9_computeTask2 :: Assertion
unit_Day9_computeTask2 = computeTask2 input @?= 1558722
