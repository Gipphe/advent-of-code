module Y2021.Day10Spec where

import Test.Tasty.HUnit
import Y2021.Day10

unit_Day10_computeTask1 :: Assertion
unit_Day10_computeTask1 = computeTask1 input @?= 193275

unit_Day10_computeTask2 :: Assertion
unit_Day10_computeTask2 = computeTask2 input @?= 2429644557
