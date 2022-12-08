module Y2022.Day7Spec where

import Test.Tasty.HUnit
import Y2022.Day7

unit_Day7_computeTask1 :: Assertion
unit_Day7_computeTask1 = computeTask1 input @?= 1644735

unit_Day7_computeTask2 :: Assertion
unit_Day7_computeTask2 = computeTask2 input @?= 1300850
