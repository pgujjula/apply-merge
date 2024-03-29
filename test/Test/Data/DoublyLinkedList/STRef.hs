-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Data.DoublyLinkedList.STRef (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Data.DoublyLinkedList.STRef"
    [ constructionTests,
      traversalTests,
      queryTests,
      insertionTests,
      deletionTests,
      listConversionTests,
      integrationTests
    ]

unimplemented :: Assertion
unimplemented = assertFailure "unimplemented"

-- Construction
constructionTests :: TestTree
constructionTests =
  testGroup
    "Construction"
    [ emptyTests,
      fromListTests
    ]

emptyTests :: TestTree
emptyTests = testCase "empty" (pure ())

fromListTests :: TestTree
fromListTests = testCase "fromList" (pure ())

-- Traversal
traversalTests :: TestTree
traversalTests =
  testGroup "Traversal" [headTests, lastTests, nextTests, prevTests]

headTests :: TestTree
headTests = ignoreTest $ testCase "head" unimplemented

lastTests :: TestTree
lastTests = ignoreTest $ testCase "last" unimplemented

nextTests :: TestTree
nextTests = ignoreTest $ testCase "next" unimplemented

prevTests :: TestTree
prevTests = ignoreTest $ testCase "prev" unimplemented

-- Query
queryTests :: TestTree
queryTests = testGroup "Query" [nullTests, valueTests]

nullTests :: TestTree
nullTests = ignoreTest $ testCase "null" unimplemented

valueTests :: TestTree
valueTests = ignoreTest $ testCase "value" unimplemented

-- Insertion
insertionTests :: TestTree
insertionTests =
  testGroup
    "Insertion"
    [ consTests,
      snocTests,
      insertBeforeTests,
      insertAfterTests
    ]

consTests :: TestTree
consTests = ignoreTest $ testCase "cons" unimplemented

snocTests :: TestTree
snocTests = ignoreTest $ testCase "snoc" unimplemented

insertBeforeTests :: TestTree
insertBeforeTests = ignoreTest $ testCase "insertBefore" unimplemented

insertAfterTests :: TestTree
insertAfterTests = ignoreTest $ testCase "insertAfter" unimplemented

-- Deletion
deletionTests :: TestTree
deletionTests = testGroup "Deletion" [deleteTests]

deleteTests :: TestTree
deleteTests = ignoreTest $ testCase "delete" unimplemented

-- List conversion
listConversionTests :: TestTree
listConversionTests = testGroup "List conversion" [toListTests]

toListTests :: TestTree
toListTests = ignoreTest $ testCase "toList" unimplemented

-- Integration
integrationTests :: TestTree
integrationTests = ignoreTest $ testCase "Integration" unimplemented
