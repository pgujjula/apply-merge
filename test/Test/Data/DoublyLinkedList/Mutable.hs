-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Data.DoublyLinkedList.Mutable (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

tests :: TestTree
tests =
  ignoreTest $
    testGroup
      "Data.DoublyLinkedList.Mutable"
      [ nodeConversionTests,
        constructionTests,
        traversalTests,
        queryTests,
        insertionTests,
        deletionTests,
        listConversionTests,
        integrationTests
      ]

-- Node conversion
nodeConversionTests :: TestTree
nodeConversionTests =
  testGroup
    "Node conversion"
    [ nextNodeToValueNodeTests,
      prevNodeToValueNodeTests
    ]

unimplemented :: Assertion
unimplemented = assertFailure "unimplemented"

nextNodeToValueNodeTests :: TestTree
nextNodeToValueNodeTests = testCase "nextNodeToValueNode" unimplemented

prevNodeToValueNodeTests :: TestTree
prevNodeToValueNodeTests = testCase "prevNodeToValueNode" unimplemented

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
traversalTests = testGroup "Traversal" [nextTests, prevTests]

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
deletionTests = testGroup "Deletion" [deleteNodeTests]

deleteNodeTests :: TestTree
deleteNodeTests = testCase "deleteNode" unimplemented

-- List conversion
listConversionTests :: TestTree
listConversionTests = testGroup "List conversion" [toListTests]

toListTests :: TestTree
toListTests = testCase "toList" unimplemented

-- Integration
integrationTests :: TestTree
integrationTests = testCase "Integration" unimplemented
