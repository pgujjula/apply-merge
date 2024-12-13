-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Data.List.ApplyMerge.New (tests) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.Kind (Type)
import Data.List (sort)
import qualified Data.List as List
import qualified Data.List.ApplyMerge as List (applyMerge, applyMergeBy, applyMergeOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty.ApplyMerge as NonEmpty
import GHC.Exts (toList)
import Numeric.Natural (Natural)
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( InfiniteList,
    getInfiniteList,
    getNonNegative,
    (===),
  )
import qualified Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "Data.List.ApplyMerge.New"
    [ genericTestApplyMerge
        "List"
        testListApplyMerge
        (List.applyMerge, List.applyMergeOn, List.applyMergeBy),
      genericTestApplyMerge
        "NonEmpty"
        testNonEmptyApplyMerge
        (NonEmpty.applyMerge, NonEmpty.applyMergeOn, NonEmpty.applyMergeBy)
    ]

genericTestApplyMerge ::
  forall f.
  (Functor f) =>
  String ->
  (ApplyMerge f -> String -> String -> TestTree) ->
  (ApplyMerge f, ApplyMergeOn f, ApplyMergeBy f) ->
  TestTree
genericTestApplyMerge label testApplyMerge (applyMerge, applyMergeOn, applyMergeBy) =
  testGroup
    label
    [ testApplyMerge applyMerge "applyMerge f xs ys" "f",
      testGroup "applyMergeOn proj f xs ys" . List.singleton $
        let applyMergeViaOn :: ApplyMerge f
            applyMergeViaOn f xs ys =
              fmap (uncurry f) (applyMergeOn (uncurry f) (,) xs ys)
         in testApplyMerge applyMergeViaOn "f = (,)" "proj",
      testGroup "applyMergeBy cmp f xs ys" . List.singleton $
        testApplyMerge (applyMergeBy compare) "cmp = compare" "f"
    ]

type ApplyMerge f =
  forall a b c. (Ord c) => (a -> b -> c) -> f a -> f b -> f c

type ApplyMergeOn f =
  forall a b c d. (Ord d) => (c -> d) -> (a -> b -> c) -> f a -> f b -> f c

type ApplyMergeBy f =
  forall a b c. (c -> c -> Ordering) -> (a -> b -> c) -> f a -> f b -> f c

testNonEmptyApplyMerge :: ApplyMerge NonEmpty -> String -> String -> TestTree
testNonEmptyApplyMerge = testGenericApplyMerge testNonEmptyFunctions

testListApplyMerge :: ApplyMerge [] -> String -> String -> TestTree
testListApplyMerge = testGenericApplyMerge testListFunctions

type TestFunctions f =
  forall a.
  (Show a, Integral a, QC.Arbitrary a) =>
  String ->
  ApplyMerge f ->
  [(String, a -> a -> a)] ->
  (a -> a -> a) ->
  TestTree

testGenericApplyMerge ::
  TestFunctions f -> ApplyMerge f -> String -> String -> TestTree
testGenericApplyMerge testGenericFunctions am label funcLabel =
  testGroup
    label
    [ testGenericFunctions
        ("increasing " <> funcLabel <> ", increasing xs and ys")
        am
        increasingNaturalFuncs
        (+),
      testGenericFunctions
        ("decreasing " <> funcLabel <> ", decreasing xs and ys")
        am
        decreasingIntegerFuncs
        (-)
    ]

increasingNaturalFuncs :: [(String, Natural -> Natural -> Natural)]
increasingNaturalFuncs =
  let xs =
        [ ("const", const),
          ("min", min),
          ("max", max),
          ("(+)", (+)),
          ("(\\x y -> 4 * x + y)", \x y -> 4 * x + y),
          ("(*)", (*)),
          ("(\\x y -> x ^ (3 :: Int) * y)", \x y -> x ^ (3 :: Int) * y),
          ("(\\x y -> x * x + y * y)", \x y -> x * x + y * y),
          ("(\\x y -> 4 * x * x + y * y)", \x y -> 4 * x * x + y * y),
          ("(\\x _ -> x `quot` 5)", \x _ -> x `quot` 5),
          ("(\\x y -> (x `quot` 5) + y)", \x y -> (x `quot` 5) + y),
          ( "(\\x y -> (x `quot` 5) + (y `quot` 5))",
            \x y -> (x `quot` 5) + (y `quot` 5)
          )
        ]
   in xs ++ map (bimap ("flip " <>) flip) xs

decreasingIntegerFuncs :: [(String, Integer -> Integer -> Integer)]
decreasingIntegerFuncs =
  let xs =
        [ ("(\\x _ -> -x)", \x _ -> -x),
          ("(\\x y -> -min x y)", \x y -> -min x y),
          ("(\\x y -> -max x y)", \x y -> -max x y),
          ("(\\x y -> -x - y)", \x y -> -x - y),
          ("(\\x y -> -(4 * x) - y)", \x y -> -(4 * x) - y),
          ("(\\x _ -> -(x `quot` 5))", \x _ -> -(x `quot` 5)),
          ("(\\x y -> -(x `quot` 5) - y)", \x y -> -(x `quot` 5) - y),
          ( "(\\x y -> -(x `quot` 5) - (y `quot` 5))",
            \x y -> -(x `quot` 5) - (y `quot` 5)
          )
        ]
   in xs ++ map (bimap ("flip " <>) flip) xs

getOrderedList :: (a -> a -> a) -> PossiblyInfinite [QC.NonNegative a] -> [a]
getOrderedList op = scanl1 op . map getNonNegative . getPossiblyInfinite

getOrderedNonEmpty ::
  (a -> a -> a) -> PossiblyInfinite (NonEmpty (QC.NonNegative a)) -> NonEmpty a
getOrderedNonEmpty op =
  NonEmpty.scanl1 op . NonEmpty.map QC.getNonNegative . getPossiblyInfinite

testListFunctions :: TestFunctions []
testListFunctions label am funcs op =
  QC.testProperty label $ do
    (fName, f) <- QC.elements funcs
    let limit = 100
    pure . QC.counterexample fName $
      \(getOrderedList op -> xs) (getOrderedList op -> ys) ->
        let actual = toList (am f xs ys)
            expected = sort $ on (liftA2 f) (take limit . toList) xs ys
         in on (===) (take limit) actual expected

testNonEmptyFunctions :: TestFunctions NonEmpty
testNonEmptyFunctions label am funcs op =
  QC.testProperty label $ do
    (fName, f) <- QC.elements funcs
    let limit = 100
    pure . QC.counterexample fName $
      \(getOrderedNonEmpty op -> xs) (getOrderedNonEmpty op -> ys) ->
        let actual = toList (am f xs ys)
            expected = sort $ on (liftA2 f) (take limit . toList) xs ys
         in on (===) (take limit) actual expected

class HasPossiblyInfinite (a :: Type) where
  type PossiblyInfinite a :: Type
  getPossiblyInfinite :: PossiblyInfinite a -> a

instance HasPossiblyInfinite [a] where
  type PossiblyInfinite [a] = Either [a] (InfiniteList a)
  getPossiblyInfinite = either id getInfiniteList

instance HasPossiblyInfinite (NonEmpty a) where
  type PossiblyInfinite (NonEmpty a) = (a, PossiblyInfinite [a])
  getPossiblyInfinite (x, xs) = x :| getPossiblyInfinite xs

-- Utilities
take1 :: Int -> NonEmpty a -> NonEmpty a
take1 n (x :| xs) = x :| take n xs
