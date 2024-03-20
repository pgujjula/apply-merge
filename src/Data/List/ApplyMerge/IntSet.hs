-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.List.ApplyMerge.IntSet (applyMerge) where

import Data.Function (on)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (foldl', unfoldr)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, fromMaybe)
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
  let frontier'' = foldl' (flip insertNode) frontier' (childrenNodes f node)
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

insertNode :: (Ord c) => Node a b c -> Frontier a b c -> Frontier a b c
insertNode node frontier =
  let (ia, ib) = node.position
   in if IntSet.member ia frontier.indexSetA
        || IntSet.member ib frontier.indexSetB
        then frontier
        else
          Frontier
            { queue = MinQueue.insert node frontier.queue,
              indexSetA = IntSet.insert ia frontier.indexSetA,
              indexSetB = IntSet.insert ib frontier.indexSetB
            }

childrenNodes :: forall a b c. (a -> b -> c) -> Node a b c -> [Node a b c]
childrenNodes f (Node (ia, ib) _ as bs) = catMaybes [childA, childB]
  where
    childA :: Maybe (Node a b c)
    childA = do
      as' <- nonEmpty (NonEmpty.tail as)
      pure (mkNode f (ia + 1, ib) as' bs)

    childB :: Maybe (Node a b c)
    childB = do
      bs' <- nonEmpty (NonEmpty.tail bs)
      pure (mkNode f (ia, ib + 1) as bs')

mkNode :: (a -> b -> c) -> (Int, Int) -> NonEmpty a -> NonEmpty b -> Node a b c
mkNode f (ia, ib) as bs =
  Node
    { position = (ia, ib),
      value = f (NonEmpty.head as) (NonEmpty.head bs),
      as = as,
      bs = bs
    }
