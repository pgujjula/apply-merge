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
  { pos :: (Int, Int),
    val :: c,
    as :: NonEmpty a,
    bs :: NonEmpty b
  }

instance (Eq c) => Eq (Node a b c) where
  (==) = (==) `on` (\node -> node.val)

instance (Ord c) => Ord (Node a b c) where
  compare = comparing (\node -> node.val)

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
  pure (node.val, frontier'')

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
  let (x, y) = node.pos
      frontier' =
        Frontier
          { queue = queue',
            indexSetA = IntSet.delete x frontier.indexSetA,
            indexSetB = IntSet.delete y frontier.indexSetB
          }
  pure (node, frontier')

insertNode :: (Ord c) => Node a b c -> Frontier a b c -> Frontier a b c
insertNode node frontier =
  let (x, y) = node.pos
   in if IntSet.member x frontier.indexSetA
        || IntSet.member y frontier.indexSetB
        then frontier
        else
          Frontier
            { queue = MinQueue.insert node frontier.queue,
              indexSetA = IntSet.insert x frontier.indexSetA,
              indexSetB = IntSet.insert y frontier.indexSetB
            }

childrenNodes :: forall a b c. (a -> b -> c) -> Node a b c -> [Node a b c]
childrenNodes f (Node (x, y) _ as bs) = catMaybes [nodeXPlus1, nodeYPlus1]
  where
    nodeXPlus1 :: Maybe (Node a b c)
    nodeXPlus1 = do
      as' <- nonEmpty (NonEmpty.tail as)
      pure (mkNode f (x + 1, y) as' bs)

    nodeYPlus1 :: Maybe (Node a b c)
    nodeYPlus1 = do
      bs' <- nonEmpty (NonEmpty.tail bs)
      pure (mkNode f (x, y + 1) as bs')

mkNode :: (a -> b -> c) -> (Int, Int) -> NonEmpty a -> NonEmpty b -> Node a b c
mkNode f (x, y) as bs =
  Node
    { pos = (x, y),
      val = f (NonEmpty.head as) (NonEmpty.head bs),
      as = as,
      bs = bs
    }
