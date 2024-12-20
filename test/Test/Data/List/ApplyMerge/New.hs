-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Data.List.ApplyMerge.New (tests) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.Kind (Constraint, Type)
import Data.List (sort)
import qualified Data.List as List
import qualified Data.List.ApplyMerge as List (applyMerge, applyMergeBy, applyMergeOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty.ApplyMerge as NonEmpty
import Numeric.Natural (Natural)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( InfiniteList,
    getInfiniteList,
    (===),
  )
import qualified Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "Data.List.ApplyMerge.New"
    [ genericTestApplyMerge
        "List"
        (List.applyMerge, List.applyMergeOn, List.applyMergeBy),
      genericTestApplyMerge
        "NonEmpty"
        (NonEmpty.applyMerge, NonEmpty.applyMergeOn, NonEmpty.applyMergeBy)
    ]

genericTestApplyMerge ::
  forall f.
  (Functor f, ApplyMergeTestable f) =>
  String ->
  (ApplyMerge f, ApplyMergeOn f, ApplyMergeBy f) ->
  TestTree
genericTestApplyMerge label (applyMerge, applyMergeOn, applyMergeBy) =
  testGroup
    label
    [ testGenericApplyMerge applyMerge "applyMerge f xs ys" "f",
      testGroup "applyMergeOn proj f xs ys" . List.singleton $
        let applyMergeViaOn :: ApplyMerge f
            applyMergeViaOn f xs ys =
              fmap (uncurry f) (applyMergeOn (uncurry f) (,) xs ys)
         in testGenericApplyMerge applyMergeViaOn "f = (,)" "proj",
      testGroup "applyMergeBy cmp f xs ys" . List.singleton $
        testGenericApplyMerge (applyMergeBy compare) "cmp = compare" "f"
    ]

type ApplyMerge f =
  forall a b c. (Ord c) => (a -> b -> c) -> f a -> f b -> f c

type ApplyMergeOn f =
  forall a b c d. (Ord d) => (c -> d) -> (a -> b -> c) -> f a -> f b -> f c

type ApplyMergeBy f =
  forall a b c. (c -> c -> Ordering) -> (a -> b -> c) -> f a -> f b -> f c

type TestFunctions f =
  forall a.
  (Show a, Integral a, QC.Arbitrary a) =>
  String ->
  ApplyMerge f ->
  [(String, a -> a -> a)] ->
  (a -> a -> a) ->
  TestTree

gtest :: (ApplyMergeTestable f) => TestFunctions f
gtest label am funcs op =
  QC.testProperty label $ do
    (fName, f) <- QC.elements funcs
    let limit = 100
    pure . QC.counterexample fName $
      \(getOrderedPossiblyInfinite op -> xs) (getOrderedPossiblyInfinite op -> ys) ->
        let actual = gToList (am f xs ys)
            expected = sort $ on (liftA2 f) (take limit . gToList) xs ys
         in on (===) (take limit) actual expected

testGenericApplyMerge ::
  (ApplyMergeTestable f) =>
  ApplyMerge f ->
  String ->
  String ->
  TestTree
testGenericApplyMerge am label funcLabel =
  testGroup
    label
    [ gtest
        ("increasing " <> funcLabel <> ", increasing xs and ys")
        am
        increasingNaturalFuncs
        (+),
      gtest
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

getOrderedPossiblyInfinite ::
  (HasInfinite1 f) =>
  (a -> a -> a) ->
  PossiblyInfinite1 f (QC.NonNegative a) ->
  f a
getOrderedPossiblyInfinite op =
  gscanl1 op . fmap QC.getNonNegative . getPossiblyInfinite1

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
data Infinite1 f a where
  InfiniteList1 :: InfiniteList a -> Infinite1 [] a
  InfiniteNonEmpty1 :: a -> InfiniteList a -> Infinite1 NonEmpty a

deriving instance (Show a) => Show (Infinite1 f a)

type Showable :: (Type -> Type) -> Constraint
type Showable f = forall a. (Show a) => Show (f a)

type Arbitrarible :: (Type -> Type) -> Constraint
type Arbitrarible f = forall a. (QC.Arbitrary a) => QC.Arbitrary (f a)

type ApplyMergeTestable f = (HasInfinite1 f, Showable f, Arbitrarible f)

class (Functor f, QC.Arbitrary1 f) => HasInfinite1 f where
  infiniteArbitrary :: (QC.Arbitrary a) => QC.Gen (Infinite1 f a)
  gscanl1 :: (a -> a -> a) -> f a -> f a
  gToList :: f a -> [a]

instance HasInfinite1 [] where
  infiniteArbitrary = InfiniteList1 <$> QC.arbitrary
  gscanl1 = scanl1
  gToList = id

instance HasInfinite1 NonEmpty where
  infiniteArbitrary = liftA2 InfiniteNonEmpty1 QC.arbitrary QC.arbitrary
  gscanl1 = NonEmpty.scanl1
  gToList = NonEmpty.toList

type PossiblyInfinite1 f a = Either (f a) (Infinite1 f a)

instance (QC.Arbitrary a, HasInfinite1 f) => QC.Arbitrary (Infinite1 f a) where
  arbitrary :: QC.Gen (Infinite1 f a)
  arbitrary = infiniteArbitrary

getPossiblyInfinite1 :: PossiblyInfinite1 f a -> f a
getPossiblyInfinite1 = \case
  Left xs -> xs
  Right xs -> getInfinite1 xs

getInfinite1 :: Infinite1 f a -> f a
getInfinite1 = \case
  InfiniteList1 xs -> getInfiniteList xs
  InfiniteNonEmpty1 x xs -> x :| getInfiniteList xs
