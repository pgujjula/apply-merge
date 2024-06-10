{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Data.List.ApplyMerge.New (tests) where

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Data.Bifunctor (bimap, second)
import Data.Function (on)
import Data.List (sort)
import Data.List qualified as List
import Data.List.ApplyMerge (applyMerge, applyMergeBy, applyMergeOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.ApplyMerge qualified as NE
import Numeric.Natural (Natural)
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (getInfiniteList, getNonNegative, (===))
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "Data.List.ApplyMerge.New"
    [ genericTestApplyMerge
        "List"
        testApplyMerge
        (applyMerge, applyMergeOn, applyMergeBy),
      genericTestApplyMerge
        "NonEmpty"
        testNEApplyMerge
        (NE.applyMerge, NE.applyMergeOn, NE.applyMergeBy)
    ]

genericTestApplyMerge ::
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
        let applyMergeViaOn f xs ys =
              fmap (uncurry f) (amOn (uncurry f) (,) xs ys)
         in testAm applyMergeViaOn "f = (,)" "proj",
      testGroup "applyMergeBy cmp f xs ys" . List.singleton $
        testAm (amBy compare) "cmp = compare" "f"
    ]

type ApplyMerge f = forall a b c. (Ord c) => (a -> b -> c) -> f a -> f b -> f c

type ApplyMergeOn f = forall a b c d. (Ord d) => (c -> d) -> (a -> b -> c) -> f a -> f b -> f c

type ApplyMergeBy f = forall a b c. (c -> c -> Ordering) -> (a -> b -> c) -> f a -> f b -> f c

testNEApplyMerge :: ApplyMerge NonEmpty -> String -> String -> TestTree
testNEApplyMerge am label funcLabel =
  testGroup
    label
    [ testNEFunctions
        ("increasing " <> funcLabel <> ", increasing xs and ys")
        am
        increasingNaturalFuncs
        (+),
      testNEFunctions
        ("decreasing " <> funcLabel <> ", decreasing xs and ys")
        am
        decreasingIntegerFuncs
        (-)
    ]

testApplyMerge :: ApplyMerge [] -> String -> String -> TestTree
testApplyMerge am label funcLabel =
  testGroup
    label
    [ testFunctions
        ("increasing " <> funcLabel <> ", increasing xs and ys")
        am
        increasingNaturalFuncs
        (+),
      testFunctions
        ("decreasing " <> funcLabel <> ", decreasing xs and ys")
        am
        decreasingIntegerFuncs
        (-)
    ]

testNEFunctions ::
  forall a.
  (Show a, Integral a, QC.Arbitrary a) =>
  String ->
  ApplyMerge NonEmpty ->
  [(String, a -> a -> a)] ->
  (a -> a -> a) ->
  TestTree
testNEFunctions label am funcs op =
  QC.testProperty label $ do
    (fName, f) <- QC.elements funcs
    let limit = 100
    let getOrderedNonEmpty ::
          ( QC.NonNegative a,
            Either [QC.NonNegative a] (QC.InfiniteList (QC.NonNegative a))
          ) ->
          NonEmpty a
        getOrderedNonEmpty =
          second (either id getInfiniteList)
            >>> uncurry (:|)
            >>> NE.map QC.getNonNegative
            >>> NE.scanl1 op
    pure . QC.counterexample fName $
      \(getOrderedNonEmpty -> xs) (getOrderedNonEmpty -> ys) ->
        let actual = am f xs ys
            expected = NE.sort $ on (liftA2 f) (take1 limit) xs ys
         in on (===) (NE.take limit) actual expected

testFunctions ::
  forall a.
  (Show a, Integral a, QC.Arbitrary a) =>
  String ->
  ApplyMerge [] ->
  [(String, a -> a -> a)] ->
  (a -> a -> a) ->
  TestTree
testFunctions label am funcs op =
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
