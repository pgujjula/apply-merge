-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Data.DoublyLinkedList.STRef (tests) where

import Control.Monad.ST (runST)
import Data.DoublyLinkedList.STRef (empty, fromList, head, value)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Prelude hiding (head)

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
headTests =
  testGroup
    "head"
    [ testCase "head of empty list" $ do
        -- Construct empty list using empty
        let firstNodeValue1 :: Maybe Int
            firstNodeValue1 = runST $ do
              list <- empty
              firstNode <- head list
              pure (value <$> firstNode)
        firstNodeValue1 @?= Nothing

        -- Construct empty list using fromList
        let firstNodeValue2 :: Maybe Int
            firstNodeValue2 = runST $ do
              list <- fromList []
              firstNode <- head list
              pure (value <$> firstNode)
        firstNodeValue2 @?= Nothing,
      testCase "head of non-empty lists" $ do
        let firstNodeValue1 :: Maybe Int
            firstNodeValue1 = runST $ do
              list <- fromList [1]
              firstNode <- head list
              pure (value <$> firstNode)
        firstNodeValue1 @?= Just 1
        let firstNodeValue2 :: Maybe Int
            firstNodeValue2 = runST $ do
              list <- fromList [2, 1]
              firstNode <- head list
              pure (value <$> firstNode)
        firstNodeValue2 @?= Just 2
        let firstNodeValue3 :: Maybe Int
            firstNodeValue3 = runST $ do
              list <- fromList [3, 2, 1]
              firstNode <- head list
              pure (value <$> firstNode)
        firstNodeValue3 @?= Just 3
    ]

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
