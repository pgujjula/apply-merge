-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.List.ApplyMerge.IntMap (applyMerge) where

import Control.Monad (when)
import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPQueue
import Optics (assign, makeLenses, modifying, use, view)

data Node a b c = Node
  { _location :: (Int, Int),
    _downList :: [a],
    _rightList :: [b]
  }

data Frontier a b c = Frontier
  { _queue :: MinPQueue c (Node a b c),
    _locationMap :: IntMap Int
  }

makeLenses ''Node
makeLenses ''Frontier

applyMerge :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge _ [] _ = []
applyMerge _ _ [] = []
applyMerge f as bs = State.evalState (generate f) (initialFrontier f as bs)

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
          _downList = as,
          _rightList = bs
        }

step :: (Ord c) => (a -> b -> c) -> State (Frontier a b c) c
step f = do
  -- Remove minimal node from queue
  q <- use queue
  let ((value, node), q') = MinPQueue.deleteFindMin q
  assign queue q'

  -- Remove minimal node from locationMap
  let (y, x) = view location node
  modifying locationMap (IntMap.delete y)

  -- Add the node below to the queue and location map
  maybeYDown <- fmap fst . IntMap.lookupGT y <$> use locationMap
  let addDown =
        maybeYDown /= Just (y + 1)
          && (not . null . tail . view downList $ node)
  when addDown $ do
    let asDown = tail . view downList $ node
        bsDown = view rightList node
        valueDown = f (head asDown) (head bsDown)
        locationDown = (y + 1, x)
        nodeDown =
          Node
            { _location = locationDown,
              _downList = asDown,
              _rightList = bsDown
            }
    modifying queue (MinPQueue.insert valueDown nodeDown)
    modifying locationMap (IntMap.insert (y + 1) x)

  -- Add the node to the right to the queue and location map
  maybeXRight <- fmap snd . IntMap.lookupLT y <$> use locationMap
  let addRight =
        maybeXRight /= Just (x + 1)
          && (not . null . tail . view rightList $ node)
  when addRight $ do
    let asRight = view downList node
        bsRight = tail . view rightList $ node
        valueRight = f (head asRight) (head bsRight)
        locationRight = (y, x + 1)
        nodeRight =
          Node
            { _location = locationRight,
              _downList = asRight,
              _rightList = bsRight
            }
    modifying queue (MinPQueue.insert valueRight nodeRight)
    modifying locationMap (IntMap.insert y (x + 1))

  pure value

generate :: (Ord c) => (a -> b -> c) -> State (Frontier a b c) [c]
generate f = do
  q <- use queue
  if MinPQueue.null q
    then pure []
    else (:) <$> step f <*> generate f
