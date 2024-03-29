-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Data.DoublyLinkedList.STRef (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

tests :: TestTree
tests =
  ignoreTest $
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
emptyTests = testCase "empty" unimplemented

fromListTests :: TestTree
fromListTests = testCase "fromList" unimplemented

-- Traversal
traversalTests :: TestTree
traversalTests =
  testGroup "Traversal" [headTests, lastTests, nextTests, prevTests]

headTests :: TestTree
headTests = testCase "head" unimplemented

lastTests :: TestTree
lastTests = testCase "last" unimplemented

nextTests :: TestTree
nextTests = testCase "next" unimplemented

prevTests :: TestTree
prevTests = testCase "prev" unimplemented

-- Query
queryTests :: TestTree
queryTests = testGroup "Query" [nullTests, valueTests]

nullTests :: TestTree
nullTests = testCase "null" unimplemented

valueTests :: TestTree
valueTests = testCase "value" unimplemented

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
consTests = testCase "cons" unimplemented

snocTests :: TestTree
snocTests = testCase "snoc" unimplemented

insertBeforeTests :: TestTree
insertBeforeTests = testCase "insertBefore" unimplemented

insertAfterTests :: TestTree
insertAfterTests = testCase "insertAfter" unimplemented

-- Deletion
deletionTests :: TestTree
deletionTests = testGroup "Deletion" [deleteTests]

deleteTests :: TestTree
deleteTests = testCase "delete" unimplemented

-- List conversion
listConversionTests :: TestTree
listConversionTests = testGroup "List conversion" [toListTests]

toListTests :: TestTree
toListTests = testCase "toList" unimplemented

-- Integration
integrationTests :: TestTree
integrationTests = testCase "Integration" unimplemented
