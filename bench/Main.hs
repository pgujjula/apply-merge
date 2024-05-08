-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import ApplyMerge.DoublyLinkedList qualified
import ApplyMerge.IntMap qualified
import ApplyMerge.IntSet qualified
import Bench.Data.DoublyLinkedList.STRef qualified
import Bench.PriorityQueue.MinPQueue qualified
import Bench.PriorityQueue.MinPQueue.Mutable qualified
import Data.Function ((&))
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, nf)

main :: IO ()
main =
  defaultMain
    [ benchCommon
        "DoublyLinkedList"
        ApplyMerge.DoublyLinkedList.applyMerge,
      benchCommon "IntMap" ApplyMerge.IntMap.applyMerge,
      benchCommon "IntSet" ApplyMerge.IntSet.applyMerge,
      Bench.Data.DoublyLinkedList.STRef.benchmarks,
      Bench.PriorityQueue.MinPQueue.benchmarks,
      Bench.PriorityQueue.MinPQueue.Mutable.benchmarks
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
