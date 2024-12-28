-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import qualified Test.ApplyMerge.DoublyLinkedList (tests)
import qualified Test.ApplyMerge.IntMap (tests)
import qualified Test.ApplyMerge.IntSet (tests)
import qualified Test.ApplyMerge.MergeAll (tests)
import qualified Test.Data.DoublyLinkedList.STRef (tests)
import qualified Test.Data.List.ApplyMerge (tests)
import qualified Test.Data.PQueue.Prio.Min.Mutable (tests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    ""
    [ Test.ApplyMerge.DoublyLinkedList.tests,
      Test.ApplyMerge.IntMap.tests,
      Test.ApplyMerge.IntSet.tests,
      Test.ApplyMerge.MergeAll.tests,
      Test.Data.List.ApplyMerge.tests,
      Test.Data.DoublyLinkedList.STRef.tests,
      Test.Data.PQueue.Prio.Min.Mutable.tests
    ]
