-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module Data.List.ApplyMerge.IntMap (applyMerge) where

import Control.Monad (when)
import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPQueue

data Node a b c = Node
  { _location :: (Int, Int),
    _as :: [a],
    _bs :: [b]
  }

data Frontier a b c = Frontier
  { _queue :: MinPQueue c (Node a b c),
    _locationMap :: IntMap Int
  }

applyMerge :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge _ [] _ = []
applyMerge _ _ [] = []
applyMerge f as bs = State.evalState (generate f) (initialFrontier f as bs)

deleteMinNode :: (Ord c) => State (Frontier a b c) (c, Node a b c)
deleteMinNode = do
  -- Remove minimal node from queue
  q <- State.gets _queue
  let ((value, node), q') = MinPQueue.deleteFindMin q
  State.modify $ \frontier' ->
    frontier' {_queue = q'}

  -- Remove minimal node from locationMap
  let (y, _) = _location node
  State.modify $ \frontier ->
    frontier
      { _locationMap = IntMap.delete y (_locationMap frontier)
      }
  pure (value, node)

initialFrontier :: (a -> b -> c) -> [a] -> [b] -> Frontier a b c
initialFrontier f as bs =
  Frontier
    { _queue = MinPQueue.singleton c node,
      _locationMap = IntMap.singleton 0 0
    }
  where
    c = f (head as) (head bs)
    node =
      Node
        { _location = (0, 0),
          _as = as,
          _bs = bs
        }

step :: (Ord c) => (a -> b -> c) -> State (Frontier a b c) c
step f = do
  (value, node) <- deleteMinNode
  let (y, x) = _location node

  -- Add the node below to the queue and location map
  maybeYDown <- State.gets (fmap fst . IntMap.lookupGT y . _locationMap)
  let addDown =
        maybeYDown /= Just (y + 1)
          && (not . null . tail . _as $ node)
  when addDown $ do
    let asDown = tail . _as $ node
        bsDown = _bs node
        valueDown = f (head asDown) (head bsDown)
        locationDown = (y + 1, x)
        nodeDown =
          Node
            { _location = locationDown,
              _as = asDown,
              _bs = bsDown
            }
    State.modify $ \frontier ->
      frontier
        { _queue = MinPQueue.insert valueDown nodeDown (_queue frontier),
          _locationMap = IntMap.insert (y + 1) x (_locationMap frontier)
        }

  -- Add the node to the right to the queue and location map
  maybeXRight <- State.gets (fmap snd . IntMap.lookupLT y . _locationMap)
  let addRight =
        maybeXRight /= Just (x + 1)
          && (not . null . tail . _bs $ node)
  when addRight $ do
    let asRight = _as node
        bsRight = tail . _bs $ node
        valueRight = f (head asRight) (head bsRight)
        locationRight = (y, x + 1)
        nodeRight =
          Node
            { _location = locationRight,
              _as = asRight,
              _bs = bsRight
            }
    State.modify $ \frontier ->
      frontier
        { _queue = MinPQueue.insert valueRight nodeRight (_queue frontier),
          _locationMap = IntMap.insert y (x + 1) (_locationMap frontier)
        }
  pure value

generate :: (Ord c) => (a -> b -> c) -> State (Frontier a b c) [c]
generate f = do
  q <- State.gets _queue
  if MinPQueue.null q
    then pure []
    else (:) <$> step f <*> generate f
