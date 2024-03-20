-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.List.ApplyMerge.IntMap (applyMerge) where

import Control.Monad (guard)
import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPQueue

data Node a b c = Node
  { position :: (Int, Int),
    _as :: [a],
    _bs :: [b]
  }

data Frontier a b c = Frontier
  { queue :: MinPQueue c (Node a b c),
    locationMap :: IntMap Int
  }

applyMerge :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge _ [] _ = []
applyMerge _ _ [] = []
applyMerge f as bs = State.evalState (generate f) (initialFrontier f as bs)

initialFrontier :: (a -> b -> c) -> [a] -> [b] -> Frontier a b c
initialFrontier f as bs =
  Frontier
    { queue = MinPQueue.singleton c node,
      locationMap = IntMap.singleton 0 0
    }
  where
    c = f (head as) (head bs)
    node = Node (0, 0) as bs

generate :: (Ord c) => (a -> b -> c) -> State (Frontier a b c) [c]
generate f = do
  q <- State.gets (.queue)
  if MinPQueue.null q
    then pure []
    else (:) <$> State.state (step f) <*> generate f

step :: (Ord c) => (a -> b -> c) -> Frontier a b c -> (c, Frontier a b c)
step f frontier = do
  let ((value, node), frontier') = deleteMinNode frontier
  let frontier'' =
        frontier'
          & insertChildA f node
          & insertChildB f node
   in (value, frontier'')

deleteMinNode :: (Ord c) => Frontier a b c -> ((c, Node a b c), Frontier a b c)
deleteMinNode frontier =
  let q = frontier.queue
      ((value, node), q') = MinPQueue.deleteFindMin q
      (y, _) = node.position
      frontier' =
        Frontier
          { locationMap = IntMap.delete y frontier.locationMap,
            queue = q'
          }
   in ((value, node), frontier')

insertChildA ::
  (Ord c) => (a -> b -> c) -> Node a b c -> Frontier a b c -> Frontier a b c
insertChildA f (Node (y, x) as bs) frontier = fromMaybe frontier $ do
  -- Add the node below to the queue and location map
  let maybeYDown = fmap fst . IntMap.lookupGT y $ frontier.locationMap
  let addDown = maybeYDown /= Just (y + 1) && (not . null . tail $ as)
  guard addDown
  let as' = tail as
      childA = Node (y + 1, x) as' bs
      value = f (head as') (head bs)
  pure $ insertNode value childA frontier

insertChildB ::
  (Ord c) => (a -> b -> c) -> Node a b c -> Frontier a b c -> Frontier a b c
insertChildB f (Node (y, x) as bs) frontier = fromMaybe frontier $ do
  let maybeXRight = fmap snd . IntMap.lookupLT y $ frontier.locationMap
  let addRight = maybeXRight /= Just (x + 1) && (not . null . tail $ bs)
  guard addRight
  let bs' = tail bs
      childB = Node (y, x + 1) as bs'
      value = f (head as) (head bs')
  pure $ insertNode value childB frontier

insertNode :: (Ord c) => c -> Node a b c -> Frontier a b c -> Frontier a b c
insertNode value node frontier =
  let (y, x) = node.position
   in Frontier
        { queue = MinPQueue.insert value node frontier.queue,
          locationMap = IntMap.insert y x frontier.locationMap
        }
