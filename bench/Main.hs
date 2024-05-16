-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Bench.ApplyMerge qualified
import Bench.Data.DoublyLinkedList.STRef qualified
import Bench.Data.PQueue.Prio.Min qualified
import Bench.Data.PQueue.Prio.Min.Mutable qualified
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain
    [ Bench.ApplyMerge.benchmarks,
      Bench.Data.DoublyLinkedList.STRef.benchmarks,
      Bench.Data.PQueue.Prio.Min.benchmarks,
      Bench.Data.PQueue.Prio.Min.Mutable.benchmarks
    ]
