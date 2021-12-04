module Y2020.Day19Spec where

import Test.Tasty.HUnit
import Y2020.Day19

unit_Day19_computeTask1 :: Assertion
unit_Day19_computeTask1 = computeTask1 parsedInput @?= 279

unit_Day19_computeTask2 :: Assertion
unit_Day19_computeTask2 = computeTask2 parsedInput @?= 384
