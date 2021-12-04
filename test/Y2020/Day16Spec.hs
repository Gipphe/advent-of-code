module Y2020.Day16Spec where

import Test.Tasty.HUnit
import Y2020.Day16

unit_Day16_computeTask1 :: Assertion
unit_Day16_computeTask1 = computeTask1 parsedInput @?= 23044

unit_Day16_computeTask2 :: Assertion
unit_Day16_computeTask2 = computeTask2 parsedInput @?= 3765150732757
