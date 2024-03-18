-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Data.List.ApplyMerge.IntMap
  ( tests,
  )
where

import Data.List.ApplyMerge.IntMap (applyMerge)
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
    "Data.List.ApplyMerge.IntMap"
    [ applyMergeBasicTest applyMerge,
      applyMergeSkewedTest applyMerge,
      applyMergeBlockTest applyMerge,
      applyMergeMaxTest applyMerge
    ]
