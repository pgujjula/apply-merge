-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Data.DoublyLinkedList.STRef (tests) where

import Control.Monad.ST (runST)
import Data.DoublyLinkedList.STRef (empty, fromList, head, last, value)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Prelude hiding (head, last)

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
lastTests =
  testGroup
    "last"
    [ testCase "last of empty list" $ do
        -- Construct empty list using empty
        let lastNodeValue1 :: Maybe Int
            lastNodeValue1 = runST $ do
              list <- empty
              lastNode <- last list
              pure (value <$> lastNode)
        lastNodeValue1 @?= Nothing

        -- Construct empty list using fromList
        let lastNodeValue2 :: Maybe Int
            lastNodeValue2 = runST $ do
              list <- fromList []
              lastNode <- last list
              pure (value <$> lastNode)
        lastNodeValue2 @?= Nothing,
      testCase "last of non-empty lists" $ do
        let lastNodeValue1 :: Maybe Int
            lastNodeValue1 = runST $ do
              list <- fromList [1]
              lastNode <- last list
              pure (value <$> lastNode)
        lastNodeValue1 @?= Just 1
        let lastNodeValue2 :: Maybe Int
            lastNodeValue2 = runST $ do
              list <- fromList [1, 2]
              lastNode <- last list
              pure (value <$> lastNode)
        lastNodeValue2 @?= Just 2
        let lastNodeValue3 :: Maybe Int
            lastNodeValue3 = runST $ do
              list <- fromList [1, 2, 3]
              lastNode <- last list
              pure (value <$> lastNode)
        lastNodeValue3 @?= Just 3
    ]

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
valueTests = testCase "value" (pure ())

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
