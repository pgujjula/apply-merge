-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.List.ApplyMerge.IntMap (applyMerge) where

import Control.Monad (guard)
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
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
    indexMap :: IntMap Int
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
          indexMap = IntMap.singleton 0 0
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
  let (ia, _) = node.position
      frontier' =
        Frontier
          { queue = queue',
            indexMap = IntMap.delete ia frontier.indexMap
          }
  pure (node, frontier')

insertChildA ::
  (Ord c) => (a -> b -> c) -> Node a b c -> Frontier a b c -> Frontier a b c
insertChildA f (Node (ia, ib) _ as bs) frontier = fromMaybe frontier $ do
  let iaNext = fmap fst . IntMap.lookupGT ia $ frontier.indexMap
  guard $ iaNext /= Just (ia + 1)
  as' <- nonEmpty (NonEmpty.tail as)
  let childA = mkNode f (ia + 1, ib) as' bs
  pure $ insertNode childA frontier

insertChildB ::
  (Ord c) => (a -> b -> c) -> Node a b c -> Frontier a b c -> Frontier a b c
insertChildB f (Node (ia, ib) _ as bs) frontier = fromMaybe frontier $ do
  let ibNext = fmap snd . IntMap.lookupLT ia $ frontier.indexMap
  guard $ ibNext /= Just (ib + 1)
  bs' <- nonEmpty (NonEmpty.tail bs)
  let childB = mkNode f (ia, ib + 1) as bs'
  pure $ insertNode childB frontier

insertNode :: (Ord c) => Node a b c -> Frontier a b c -> Frontier a b c
insertNode node frontier =
  let (ia, ib) = node.position
   in Frontier
        { queue = MinPQueue.insert node.value node frontier.queue,
          indexMap = IntMap.insert ia ib frontier.indexMap
        }

mkNode :: (a -> b -> c) -> (Int, Int) -> NonEmpty a -> NonEmpty b -> Node a b c
mkNode f (ia, ib) as bs =
  Node
    { position = (ia, ib),
      value = f (NonEmpty.head as) (NonEmpty.head bs),
      as = as,
      bs = bs
    }
