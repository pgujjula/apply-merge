{-# LANGUAGE ViewPatterns #-}

module Test.Data.List.ApplyMerge.New (tests) where

import Control.Applicative (liftA2)
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (sort)
import Data.List qualified as List
import Data.List.ApplyMerge (applyMerge, applyMergeBy, applyMergeOn)
import Numeric.Natural (Natural)
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (getInfiniteList, (===), getNonNegative)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "Data.List.ApplyMerge.New"
    [ testGroup
        "List"
        [ testApplyMerge applyMerge "applyMerge f xs ys" "f",
          testGroup "applyMergeOn proj f xs ys" . List.singleton $
            let applyMergeViaOn :: ApplyMerge
                applyMergeViaOn f xs ys =
                  map (uncurry f) (applyMergeOn (uncurry f) (,) xs ys)
             in testApplyMerge applyMergeViaOn "f = (,)" "proj",
          testGroup "applyMerge cmp f xs ys" . List.singleton $
            testApplyMerge (applyMergeBy compare) "cmp = compare" "f"
        ]
    ]

type ApplyMerge = forall a b c. (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]

testApplyMerge :: ApplyMerge -> String -> String -> TestTree
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

testFunctions ::
  forall a.
  (Show a, Integral a, QC.Arbitrary a) =>
  String ->
  ApplyMerge ->
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
