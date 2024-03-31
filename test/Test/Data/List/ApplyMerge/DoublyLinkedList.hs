-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module Test.Data.List.ApplyMerge.DoublyLinkedList
  ( tests,
  )
where

import Data.List.ApplyMerge.DoublyLinkedList (applyMerge)
import Test.Data.List.ApplyMerge.Common
  ( basicTest,
    blockTest,
    maxTest,
    skewedTest,
  )
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Data.List.ApplyMerge.DoublyLinkedList"
    [ basicTest applyMerge,
      skewedTest applyMerge,
      blockTest applyMerge,
      maxTest applyMerge
    ]
