-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module ApplyMerge.IntSet (applyMerge) where

import Control.Monad (guard)
import Data.Function ((&))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPQueue

data Node a b c = Node
  { position :: (Int, Int),
    value :: c,
    as :: NonEmpty a,
    bs :: NonEmpty b
  }

data Frontier a b c = Frontier
  { queue :: MinPQueue c (Node a b c),
    indexSetA :: IntSet,
    indexSetB :: IntSet
  }

applyMerge :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge f as bs = fromMaybe [] $ do
  as' <- nonEmpty as
  bs' <- nonEmpty bs
  pure (unfoldr (step f) (initialFrontier f as' bs'))

initialFrontier :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> Frontier a b c
initialFrontier f as bs =
  let node = mkNode f (0, 0) as bs
   in Frontier
        { queue = MinPQueue.singleton node.value node,
          indexSetA = IntSet.singleton 0,
          indexSetB = IntSet.singleton 0
        }

step :: (Ord c) => (a -> b -> c) -> Frontier a b c -> Maybe (c, Frontier a b c)
step f frontier = do
  (node, frontier') <- deleteMinNode frontier
  let frontier'' =
        frontier'
          & insertChildA f node
          & insertChildB f node
  pure (node.value, frontier'')

deleteMinNode :: (Ord c) => Frontier a b c -> Maybe (Node a b c, Frontier a b c)
deleteMinNode frontier = do
  (node, queue') <- MinPQueue.minView frontier.queue
  let (ia, ib) = node.position
      frontier' =
        Frontier
          { queue = queue',
            indexSetA = IntSet.delete ia frontier.indexSetA,
            indexSetB = IntSet.delete ib frontier.indexSetB
          }
  pure (node, frontier')

insertChildA ::
  (Ord c) => (a -> b -> c) -> Node a b c -> Frontier a b c -> Frontier a b c
insertChildA f (Node (ia, ib) _ as bs) frontier = fromMaybe frontier $ do
  guard (not (IntSet.member (ia + 1) frontier.indexSetA))
  as' <- nonEmpty (NonEmpty.tail as)
  let childA = mkNode f (ia + 1, ib) as' bs
  pure $ insertNode childA frontier

insertChildB ::
  (Ord c) => (a -> b -> c) -> Node a b c -> Frontier a b c -> Frontier a b c
insertChildB f (Node (ia, ib) _ as bs) frontier = fromMaybe frontier $ do
  guard (not (IntSet.member (ib + 1) frontier.indexSetB))
  bs' <- nonEmpty (NonEmpty.tail bs)
  let childB = mkNode f (ia, ib + 1) as bs'
  pure $ insertNode childB frontier

insertNode :: (Ord c) => Node a b c -> Frontier a b c -> Frontier a b c
insertNode node frontier =
  let (ia, ib) = node.position
   in Frontier
        { queue = MinPQueue.insert node.value node frontier.queue,
          indexSetA = IntSet.insert ia frontier.indexSetA,
          indexSetB = IntSet.insert ib frontier.indexSetB
        }

mkNode :: (a -> b -> c) -> (Int, Int) -> NonEmpty a -> NonEmpty b -> Node a b c
mkNode f (ia, ib) as bs =
  Node
    { position = (ia, ib),
      value = f (NonEmpty.head as) (NonEmpty.head bs),
      as = as,
      bs = bs
    }
