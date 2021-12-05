module Y2021.Day4Spec where

import Test.Tasty.HUnit
import Y2021.Day4

unit_Day4_computeTask1 :: Assertion
unit_Day4_computeTask1 = computeTask1 input @?= 55770

unit_Day4_computeTask2 :: Assertion
unit_Day4_computeTask2 = computeTask2 input @?= 2980
