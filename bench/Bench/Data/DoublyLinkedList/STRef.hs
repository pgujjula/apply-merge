-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Bench.Data.DoublyLinkedList.STRef (benchmarks) where

import Test.Tasty.Bench (Benchmark, bgroup)
import Prelude hiding (last, null)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Data.DoublyLinkedList.STRef"
    [ constructionBenchmarks,
      traversalBenchmarks,
      queryBenchmarks,
      insertionBenchmarks,
      deletionBenchmarks,
      listConversionBenchmarks
    ]

-- * Construction

constructionBenchmarks :: Benchmark
constructionBenchmarks =
  bgroup
    "Construction"
    [ emptyBenchmarks,
      fromListBenchmarks
    ]

emptyBenchmarks :: Benchmark
emptyBenchmarks = bgroup "empty" []

fromListBenchmarks :: Benchmark
fromListBenchmarks = bgroup "fromList" []

-- * Traversal

traversalBenchmarks :: Benchmark
traversalBenchmarks =
  bgroup
    "Traversal"
    [ headBenchmarks,
      lastBenchmarks,
      nextBenchmarks,
      prevBenchmarks
    ]

headBenchmarks :: Benchmark
headBenchmarks = bgroup "head" []

lastBenchmarks :: Benchmark
lastBenchmarks = bgroup "last" []

nextBenchmarks :: Benchmark
nextBenchmarks = bgroup "next" []

prevBenchmarks :: Benchmark
prevBenchmarks = bgroup "prev" []

-- * Query

queryBenchmarks :: Benchmark
queryBenchmarks = bgroup "Query" [nullBenchmarks, valueBenchmarks]

nullBenchmarks :: Benchmark
nullBenchmarks = bgroup "null" []

valueBenchmarks :: Benchmark
valueBenchmarks = bgroup "value" []

-- * Insertion

insertionBenchmarks :: Benchmark
insertionBenchmarks =
  bgroup
    "Insertion"
    [ consBenchmarks,
      snocBenchmarks,
      insertAfterBenchmarks,
      insertBeforeBenchmarks
    ]

consBenchmarks :: Benchmark
consBenchmarks = bgroup "cons" []

snocBenchmarks :: Benchmark
snocBenchmarks = bgroup "snoc" []

insertBeforeBenchmarks :: Benchmark
insertBeforeBenchmarks = bgroup "insertBefore" []

insertAfterBenchmarks :: Benchmark
insertAfterBenchmarks = bgroup "insertAfter" []

-- * Deletion

deletionBenchmarks :: Benchmark
deletionBenchmarks = bgroup "Deletion" [deleteBenchmarks]

deleteBenchmarks :: Benchmark
deleteBenchmarks = bgroup "delete" []

-- * List conversion

listConversionBenchmarks :: Benchmark
listConversionBenchmarks = bgroup "List conversion" [toListBenchmarks]

toListBenchmarks :: Benchmark
toListBenchmarks = bgroup "toList" []
