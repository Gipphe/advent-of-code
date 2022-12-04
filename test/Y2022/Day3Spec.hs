module Y2022.Day3Spec where

import Test.Tasty.HUnit
import Y2022.Day3

unit_Day3_computeTask1 :: Assertion
unit_Day3_computeTask1 = computeTask1 input @?= 7766

unit_Day3_computeTask2 :: Assertion
unit_Day3_computeTask2 = computeTask2 input @?= 2415
