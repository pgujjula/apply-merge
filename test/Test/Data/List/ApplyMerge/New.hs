-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
--
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Data.List.ApplyMerge.New (tests) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Arrow ((>>>))
import Data.Bifunctor (bimap, second)
import Data.Function (on)
import Data.Kind (Type)
import Data.List (sort)
import Data.List qualified as List
import Data.List.ApplyMerge (applyMerge, applyMergeBy, applyMergeOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty.ApplyMerge qualified as NE
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary,
    Gen,
    InfiniteList,
    NonNegative,
    arbitrary,
    genericShrink,
    getInfiniteList,
    getNonNegative,
    (===),
  )
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "Data.List.ApplyMerge.New"
    [ genericTestApplyMerge
        "List"
        testListApplyMerge
        (applyMerge, applyMergeOn, applyMergeBy),
      genericTestApplyMerge
        "NonEmpty"
        testNonEmptyApplyMerge
        (NE.applyMerge, NE.applyMergeOn, NE.applyMergeBy)
    ]

genericTestApplyMerge ::
  forall f.
  (Functor f) =>
  String ->
  (ApplyMerge f -> String -> String -> TestTree) ->
  (ApplyMerge f, ApplyMergeOn f, ApplyMergeBy f) ->
  TestTree
genericTestApplyMerge label testAm (am, amOn, amBy) =
  testGroup
    label
    [ testAm am "applyMerge f xs ys" "f",
      testGroup "applyMergeOn proj f xs ys" . List.singleton $
        let applyMergeViaOn :: ApplyMerge f
            applyMergeViaOn f xs ys =
              fmap (uncurry f) (amOn (uncurry f) (,) xs ys)
         in testAm applyMergeViaOn "f = (,)" "proj",
      testGroup "applyMergeBy cmp f xs ys" . List.singleton $
        testAm (amBy compare) "cmp = compare" "f"
    ]

type ApplyMerge f =
  forall a b c. (Ord c) => (a -> b -> c) -> f a -> f b -> f c

type ApplyMergeOn f =
  forall a b c d. (Ord d) => (c -> d) -> (a -> b -> c) -> f a -> f b -> f c

type ApplyMergeBy f =
  forall a b c. (c -> c -> Ordering) -> (a -> b -> c) -> f a -> f b -> f c

data family Ordered (f :: Type -> Type)

data instance Ordered []
  = OrderedList
  { olOrigin :: Integer,
    olSequence :: Either [Natural] (InfiniteList Natural)
  }
  deriving (Generic)

instance Arbitrary (Ordered []) where
  arbitrary = liftA2 OrderedList arbitrary arbitrary
  shrink = genericShrink

data instance Ordered NonEmpty
  = OrderedNonEmpty
  { onOrigin :: Integer,
    onSequence :: (Natural, Either [Natural] (InfiniteList Natural))
  }
  deriving (Generic)

instance Arbitrary (Ordered NonEmpty) where
  arbitrary = liftA2 OrderedNonEmpty arbitrary arbitrary
  shrink = genericShrink

class (Functor f) => ApplyMergeable (f :: Type -> Type) where
  getIncreasing :: Ordered f -> f Natural
  getDecreasing :: Ordered f -> f Integer
  toList :: f a -> [a]

instance ApplyMergeable [] where
  getIncreasing =
    olSequence
      >>> either id getInfiniteList
      >>> scanl1 (+)

  getDecreasing ol = 
    let x0 = olOrigin ol
        xs = map (negate . toInteger) (getIncreasing ol)
     in map (+ x0) xs

  toList = id

instance ApplyMergeable NonEmpty where
  getIncreasing =
    onSequence
      >>> second (either id getInfiniteList)
      >>> uncurry (:|)
      >>> NonEmpty.scanl1 (+)

  getDecreasing one =
    let x0 = onOrigin one
        xs = fmap (negate . toInteger) (getIncreasing one)
     in fmap (+ x0) xs

  toList = NonEmpty.toList

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

testGenericFunctions :: (Arbitrary (Ordered f), Show (Ordered f)) => ApplyMergeable f => TestFunctions f
testGenericFunctions label am _ _ =
  QC.testProperty label $ do
    (fName, f) <- QC.elements increasingNaturalFuncs
    let limit = 100
    pure . QC.counterexample fName $
      \(getIncreasing -> xs) (getIncreasing -> ys) ->
        let actual :: [Natural]
            actual = toList (am f xs ys)

            expected :: [Natural]
            expected = sort $ on (liftA2 f) (take limit) (toList xs) (toList ys)
         in on (===) (take limit) actual expected
          
testListFunctions :: TestFunctions []
testListFunctions label am funcs op =
  QC.testProperty label $ do
    (fName, f) <- QC.elements funcs
    let limit = 100
    let getOrderedList =
          scanl1 op . map getNonNegative . either id getInfiniteList
    pure . QC.counterexample fName $
      \(getOrderedList -> xs) (getOrderedList -> ys) ->
        let actual = am f xs ys
            expected = sort $ on (liftA2 f) (take limit) xs ys
         in on (===) (take limit) actual expected

testNonEmptyFunctions :: TestFunctions NonEmpty
testNonEmptyFunctions label am funcs op =
  QC.testProperty label $ do
    (fName, f) <- QC.elements funcs
    let limit = 100
    let getOrderedNonEmpty =
          second (either id getInfiniteList)
            >>> uncurry (:|)
            >>> NE.map QC.getNonNegative
            >>> NE.scanl1 op
    pure . QC.counterexample fName $
      \(getOrderedNonEmpty -> xs) (getOrderedNonEmpty -> ys) ->
        let actual = am f xs ys
            expected = NE.sort $ on (liftA2 f) (take1 limit) xs ys
         in on (===) (NE.take limit) actual expected

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

-- Utilities
take1 :: Int -> NonEmpty a -> NonEmpty a
take1 n (x :| xs) = x :| take n xs
