-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import qualified Bench.ApplyMerge
import qualified Bench.Data.DoublyLinkedList.STRef
import qualified Bench.Data.PQueue.Prio.Min
import qualified Bench.Data.PQueue.Prio.Min.Mutable
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain
    [ Bench.ApplyMerge.benchmarks,
      Bench.Data.DoublyLinkedList.STRef.benchmarks,
      Bench.Data.PQueue.Prio.Min.benchmarks,
      Bench.Data.PQueue.Prio.Min.Mutable.benchmarks
    ]
