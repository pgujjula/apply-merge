-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.List.ApplyMerge.IntSet (applyMerge) where

import Control.Monad (guard)
import Data.Function (on, (&))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.PQueue.Min (MinQueue)
import Data.PQueue.Min qualified as MinQueue

data Node a b c = Node
  { position :: (Int, Int),
    value :: c,
    as :: NonEmpty a,
    bs :: NonEmpty b
  }

instance (Eq c) => Eq (Node a b c) where
  (==) = (==) `on` (\node -> node.value)

instance (Ord c) => Ord (Node a b c) where
  compare = comparing (\node -> node.value)

data Frontier a b c = Frontier
  { queue :: MinQueue (Node a b c),
    indexSetA :: IntSet,
    indexSetB :: IntSet
  }

applyMerge :: forall a b c. (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge f as bs = fromMaybe [] $ do
  as' <- nonEmpty as
  bs' <- nonEmpty bs
  pure (unfoldr (step f) (initialFrontier f as' bs'))

step :: (Ord c) => (a -> b -> c) -> Frontier a b c -> Maybe (c, Frontier a b c)
step f frontier = do
  (node, frontier') <- deleteMinNode frontier
  let frontier'' =
        frontier'
          & insertChildA f node
          & insertChildB f node
  pure (node.value, frontier'')

initialFrontier :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> Frontier a b c
initialFrontier f as bs =
  let initialNode = mkNode f (0, 0) as bs
   in Frontier
        { queue = MinQueue.singleton initialNode,
          indexSetA = IntSet.singleton 0,
          indexSetB = IntSet.singleton 0
        }

deleteMinNode :: (Ord c) => Frontier a b c -> Maybe (Node a b c, Frontier a b c)
deleteMinNode frontier = do
  (node, queue') <- MinQueue.minView frontier.queue
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
        { queue = MinQueue.insert node frontier.queue,
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
