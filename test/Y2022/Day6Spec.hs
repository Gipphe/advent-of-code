module Y2022.Day6Spec where

import Test.Tasty.HUnit
import Y2022.Day6

unit_Day6_computeTask1 :: Assertion
unit_Day6_computeTask1 = computeTask1 input @?= 1848

unit_Day6_computeTask2 :: Assertion
unit_Day6_computeTask2 = computeTask2 input @?= 2308
