module Y2022.Day2Spec where

import Test.Tasty.HUnit
import Y2022.Day2

unit_Day2_computeTask1 :: Assertion
unit_Day2_computeTask1 = computeTask1 input @?= 12772

unit_Day2_computeTask2 :: Assertion
unit_Day2_computeTask2 = computeTask2 input @?= 11618
