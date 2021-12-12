module Y2021.Day11Spec where

import Test.Tasty.HUnit
import Y2021.Day11

unit_Day11_computeTask1 :: Assertion
unit_Day11_computeTask1 = computeTask1 input @?= 1588

unit_Day11_computeTask2 :: Assertion
unit_Day11_computeTask2 = computeTask2 input @?= 517
