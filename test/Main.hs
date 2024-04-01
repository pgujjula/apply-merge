-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Test.Data.DoublyLinkedList.STRef qualified (tests)
import Test.Data.List.ApplyMerge.DoublyLinkedList qualified (tests)
import Test.Data.List.ApplyMerge.IntMap qualified (tests)
import Test.Data.List.ApplyMerge.IntSet qualified (tests)
import Test.Data.PQueue.Prio.Min.Mutable qualified (tests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    ""
    [ Test.Data.DoublyLinkedList.STRef.tests,
      Test.Data.List.ApplyMerge.DoublyLinkedList.tests,
      Test.Data.List.ApplyMerge.IntMap.tests,
      Test.Data.List.ApplyMerge.IntSet.tests,
      Test.Data.PQueue.Prio.Min.Mutable.tests
    ]
