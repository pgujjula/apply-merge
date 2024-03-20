-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.List.ApplyMerge.IntMap (applyMerge) where

import Control.Monad (guard, when)
import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPQueue

data Node a b c = Node
  { position :: (Int, Int),
    as :: [a],
    bs :: [b]
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
    node =
      Node
        { position = (0, 0),
          as = as,
          bs = bs
        }

generate :: (Ord c) => (a -> b -> c) -> State (Frontier a b c) [c]
generate f = do
  q <- State.gets (.queue)
  if MinPQueue.null q
    then pure []
    else (:) <$> step f <*> generate f

step :: (Ord c) => (a -> b -> c) -> State (Frontier a b c) c
step f = do
  (value, node) <- State.state deleteMinNode
  State.modify' $ insertChildA f node
  insertChildB f node
  pure value

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
insertChildA f node frontier = fromMaybe frontier $ do
  let (y, x) = node.position
  -- Add the node below to the queue and location map
  let maybeYDown = fmap fst . IntMap.lookupGT y $ frontier.locationMap
  let addDown = maybeYDown /= Just (y + 1) && (not . null . tail $ node.as)
  guard addDown
  let asDown = tail node.as
      bsDown = node.bs
      valueDown = f (head asDown) (head bsDown)
      locationDown = (y + 1, x)
      nodeDown =
        Node
          { position = locationDown,
            as = asDown,
            bs = bsDown
          }
  pure $
    Frontier
      { queue = MinPQueue.insert valueDown nodeDown frontier.queue,
        locationMap = IntMap.insert (y + 1) x frontier.locationMap
      }

insertChildB ::
  (Ord c) => (a -> b -> c) -> Node a b c -> State (Frontier a b c) ()
insertChildB f node = do
  let (y, x) = node.position
  maybeXRight <- State.gets (fmap snd . IntMap.lookupLT y . (.locationMap))
  let addRight =
        maybeXRight /= Just (x + 1)
          && (not . null . tail $ node.bs)
  when addRight $ do
    let asRight = node.as
        bsRight = tail node.bs
        valueRight = f (head asRight) (head bsRight)
        locationRight = (y, x + 1)
        nodeRight =
          Node
            { position = locationRight,
              as = asRight,
              bs = bsRight
            }
    State.modify $ \frontier ->
      frontier
        { queue = MinPQueue.insert valueRight nodeRight frontier.queue,
          locationMap = IntMap.insert y (x + 1) frontier.locationMap
        }
