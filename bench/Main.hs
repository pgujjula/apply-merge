-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Data.Function ((&))
import Data.List.ApplyMerge.DoublyLinkedList qualified
import Data.List.ApplyMerge.IntMap qualified
import Data.List.ApplyMerge.IntSet qualified
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, nf)

main :: IO ()
main =
  defaultMain
    [ benchCommon
        "DoublyLinkedList"
        Data.List.ApplyMerge.DoublyLinkedList.applyMerge,
      benchCommon "IntMap" Data.List.ApplyMerge.IntMap.applyMerge,
      benchCommon "IntSet" Data.List.ApplyMerge.IntSet.applyMerge
    ]

benchCommon ::
  String ->
  (forall a b c. (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]) ->
  Benchmark
benchCommon name applyMerge =
  bgroup name [benchmarkSymmetric]
  where
    benchmarkSymmetric :: Benchmark
    benchmarkSymmetric =
      bgroup "benchmarkSymmetric" (fmap mkBench [1 .. (6 :: Int)])
      where
        mkBench :: Int -> Benchmark
        mkBench i = bench (show i) (nf collapse (10 ^ i))
          where
            collapse :: Int -> Int
            collapse n =
              let start = (n `quot` maxBound) + 1
               in applyMerge (*) [start ..] [start ..]
                    & take n
                    & sum
