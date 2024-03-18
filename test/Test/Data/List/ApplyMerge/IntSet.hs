-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module Test.Data.List.ApplyMerge.IntSet
  ( tests,
  )
where

import Data.List.ApplyMerge.IntSet (applyMerge)
import Test.Data.List.ApplyMerge.Common
  ( applyMergeBasicTest,
    applyMergeBlockTest,
    applyMergeMaxTest,
    applyMergeSkewedTest,
  )
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Data.List.ApplyMerge.IntSet"
    [ applyMergeBasicTest applyMerge,
      applyMergeSkewedTest applyMerge,
      applyMergeBlockTest applyMerge,
      applyMergeMaxTest applyMerge
    ]
