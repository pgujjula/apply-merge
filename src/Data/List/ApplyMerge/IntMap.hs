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
    _as :: NonEmpty a,
    _bs :: NonEmpty b
  }

data Frontier a b c = Frontier
  { queue :: MinPQueue c (Node a b c),
    locationMap :: IntMap Int
  }

applyMerge :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge f as bs = fromMaybe [] $ do
  as' <- nonEmpty as
  bs' <- nonEmpty bs
  pure (unfoldr (step f) (initialFrontier f as' bs'))

initialFrontier :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> Frontier a b c
initialFrontier f as bs =
  Frontier
    { queue = MinPQueue.singleton c node,
      locationMap = IntMap.singleton 0 0
    }
  where
    c = f (NonEmpty.head as) (NonEmpty.head bs)
    node = Node (0, 0) as bs

step ::
  (Ord c) => (a -> b -> c) -> Frontier a b c -> Maybe (c, Frontier a b c)
step f frontier = do
  ((value, node), frontier') <- deleteMinNode frontier
  let frontier'' =
        frontier'
          & insertChildA f node
          & insertChildB f node
  pure (value, frontier'')

deleteMinNode :: (Ord c) => Frontier a b c -> Maybe ((c, Node a b c), Frontier a b c)
deleteMinNode frontier = do
  ((value, node), q') <- MinPQueue.minViewWithKey frontier.queue
  let (y, _) = node.position
      frontier' =
        Frontier
          { locationMap = IntMap.delete y frontier.locationMap,
            queue = q'
          }
  pure ((value, node), frontier')

insertChildA ::
  (Ord c) => (a -> b -> c) -> Node a b c -> Frontier a b c -> Frontier a b c
insertChildA f (Node (y, x) as bs) frontier = fromMaybe frontier $ do
  -- Add the node below to the queue and location map
  let maybeYDown = fmap fst . IntMap.lookupGT y $ frontier.locationMap
  guard $ maybeYDown /= Just (y + 1)
  as' <- nonEmpty (NonEmpty.tail as)
  let childA = Node (y + 1, x) as' bs
      value = f (NonEmpty.head as') (NonEmpty.head bs)
  pure $ insertNode value childA frontier

insertChildB ::
  (Ord c) => (a -> b -> c) -> Node a b c -> Frontier a b c -> Frontier a b c
insertChildB f (Node (y, x) as bs) frontier = fromMaybe frontier $ do
  let maybeXRight = fmap snd . IntMap.lookupLT y $ frontier.locationMap
  guard $ maybeXRight /= Just (x + 1)
  bs' <- nonEmpty (NonEmpty.tail bs)
  let childB = Node (y, x + 1) as bs'
      value = f (NonEmpty.head as) (NonEmpty.head bs')
  pure $ insertNode value childB frontier

insertNode :: (Ord c) => c -> Node a b c -> Frontier a b c -> Frontier a b c
insertNode value node frontier =
  let (y, x) = node.position
   in Frontier
        { queue = MinPQueue.insert value node frontier.queue,
          locationMap = IntMap.insert y x frontier.locationMap
        }
