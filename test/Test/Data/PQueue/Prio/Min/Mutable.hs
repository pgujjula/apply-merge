-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Data.PQueue.Prio.Min.Mutable (tests) where

import Control.Monad (forM_, replicateM)
import Control.Monad.ST (runST)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.PQueue.Prio.Min.Mutable
import Test.Falsify.Generator (int, list)
import Test.Falsify.Predicate (eq, (.$))
import Test.Falsify.Property (Property, assert, gen, info)
import Test.Falsify.Range (between)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)

tests :: TestTree
tests =
  testGroup
    "Data.PQueue.Prio.Min.Mutable"
    [ testProperty "priority queue sorts a list" prop_sorts_list
    ]

prop_sorts_list :: Property ()
prop_sorts_list = do
  xs <- gen $ list (between (1, 20)) (int (between (0, 100)))
  info ("xs == " ++ show xs)
  let actual = pqueueSort xs
  let expected = sort xs
  assert (eq .$ ("expected", expected) .$ ("actual", actual))

pqueueSort :: (Ord a) => [a] -> [a]
pqueueSort xs = runST $ do
  queue <- empty
  forM_ xs $ \x -> insert x () queue
  map fst . catMaybes <$> replicateM (length xs) (deleteMin queue)
