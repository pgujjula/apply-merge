module Test.Data.List.ApplyMerge.New (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Data.List.ApplyMerge.New"
    [ testCase "placeholder" ((1 :: Int) @?= 1)
    ]
