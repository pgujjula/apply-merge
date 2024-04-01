-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.List.ApplyMerge.DoublyLinkedList (applyMerge) where

import Control.Monad (guard)
import Control.Monad.ST qualified as Strict
import Control.Monad.ST.Lazy qualified as Lazy
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.DoublyLinkedList.Mutable qualified as DoublyLinked
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPQueue

data Node s a b c = Node
  { position :: DoublyLinked.DoublyLinkedNode s (Int, Int),
    value :: c,
    as :: NonEmpty a,
    bs :: NonEmpty b
  }

newtype Frontier s a b c = Frontier
  { queue :: MinPQueue c (Node s a b c)
  }

applyMerge :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge f as bs =
  fromMaybe [] $
    applyMergeNonEmpty f <$> nonEmpty as <*> nonEmpty bs

applyMergeNonEmpty ::
  (Ord c) => (a -> b -> c) -> NonEmpty a -> NonEmpty b -> [c]
applyMergeNonEmpty f as bs = Lazy.runST $ do
  frontier <- Lazy.strictToLazyST (initialFrontier f as bs)
  unfoldrM (Lazy.strictToLazyST . step f) frontier

unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f seed = do
  result <- f seed
  case result of
    Nothing -> pure []
    Just (x, newSeed) -> (x :) <$> unfoldrM f newSeed

initialFrontier ::
  (a -> b -> c) -> NonEmpty a -> NonEmpty b -> Strict.ST s (Frontier s a b c)
initialFrontier f as bs = do
  list <- DoublyLinked.empty
  position <- DoublyLinked.cons list (0 :: Int, 0 :: Int)
  let c = f (NonEmpty.head as) (NonEmpty.head bs)
      node =
        Node
          { position = position,
            value = c,
            as = as,
            bs = bs
          }
  pure $ Frontier $ MinPQueue.singleton c node

step ::
  (Ord c) =>
  (a -> b -> c) ->
  Frontier s a b c ->
  Strict.ST s (Maybe (c, Frontier s a b c))
step f frontier = runMaybeT $ do
  (node, frontier') <- MaybeT (deleteMinNode frontier)
  frontier'' <- lift $ insertChildA f node frontier'
  frontier''' <- lift $ insertChildB f node frontier''
  lift (DoublyLinked.delete node.position)
  pure (node.value, frontier''')

deleteMinNode ::
  (Ord c) => Frontier s a b c -> Strict.ST s (Maybe (Node s a b c, Frontier s a b c))
deleteMinNode frontier = runMaybeT $ do
  (node, queue') <- MaybeT (pure (MinPQueue.minView frontier.queue))
  let frontier' = Frontier queue'
  pure (node, frontier')

nextNodeValue :: DoublyLinked.DoublyLinkedNode s a -> Strict.ST s (Maybe a)
nextNodeValue valueNode = runMaybeT $ do
  valueNode' <- MaybeT $ DoublyLinked.next valueNode
  pure (DoublyLinked.value valueNode')

prevNodeValue :: DoublyLinked.DoublyLinkedNode s a -> Strict.ST s (Maybe a)
prevNodeValue valueNode = runMaybeT $ do
  valueNode' <- MaybeT $ DoublyLinked.prev valueNode
  pure (DoublyLinked.value valueNode')

insertChildA ::
  (Ord c) =>
  (a -> b -> c) ->
  Node s a b c ->
  Frontier s a b c ->
  Strict.ST s (Frontier s a b c)
insertChildA f node frontier = fmap (fromMaybe frontier) $ runMaybeT $ do
  let (ia, ib) = DoublyLinked.value node.position
  nextPosition <- lift $ nextNodeValue node.position
  guard (fmap fst nextPosition /= Just (ia + 1))
  as' <- hoistMaybe (nonEmpty (NonEmpty.tail node.as))
  let bs' = node.bs
  position' <- lift (DoublyLinked.insertAfter node.position (ia + 1, ib))
  let value' = f (NonEmpty.head as') (NonEmpty.head bs')
  let node' =
        Node
          { position = position',
            value = value',
            as = as',
            bs = bs'
          }
  pure $ Frontier $ MinPQueue.insert value' node' frontier.queue

insertChildB ::
  (Ord c) =>
  (a -> b -> c) ->
  Node s a b c ->
  Frontier s a b c ->
  Strict.ST s (Frontier s a b c)
insertChildB f node frontier = fmap (fromMaybe frontier) $ runMaybeT $ do
  let (ia, ib) = DoublyLinked.value node.position
  prevPosition <- lift $ prevNodeValue node.position
  guard (fmap snd prevPosition /= Just (ib + 1))
  bs' <- hoistMaybe (nonEmpty (NonEmpty.tail node.bs))
  let as' = node.as
  position' <- lift (DoublyLinked.insertBefore node.position (ia, ib + 1))
  let value' = f (NonEmpty.head as') (NonEmpty.head bs')
  let node' = mkNode f position' as' bs'
  pure $ Frontier $ MinPQueue.insert value' node' frontier.queue

mkNode ::
  (a -> b -> c) ->
  DoublyLinked.DoublyLinkedNode s (Int, Int) ->
  NonEmpty a ->
  NonEmpty b ->
  Node s a b c
mkNode f position as bs =
  Node
    { position = position,
      value = f (NonEmpty.head as) (NonEmpty.head bs),
      as = as,
      bs = bs
    }
