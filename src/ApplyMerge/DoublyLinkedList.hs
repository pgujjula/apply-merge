-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module ApplyMerge.DoublyLinkedList (applyMerge, applyMergeNonEmpty) where

import Control.Monad (guard, (>=>))
import qualified Control.Monad.ST as Strict
import qualified Control.Monad.ST.Lazy as Lazy
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.DoublyLinkedList.STRef as DoublyLinked
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as MinPQueue

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
applyMerge f as bs = fromMaybe [] $ do
  as' <- nonEmpty as
  bs' <- nonEmpty bs
  pure (NonEmpty.toList (applyMergeNonEmpty f as' bs'))

applyMergeNonEmpty ::
  (Ord c) => (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
applyMergeNonEmpty f as bs = Lazy.runST $ do
  (c, frontier) <- Lazy.strictToLazyST (initialState f as bs)
  (c :|) <$> unfoldrM (Lazy.strictToLazyST . runMaybeT . step f) frontier

unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f seed = do
  result <- f seed
  case result of
    Nothing -> pure []
    Just (x, newSeed) -> (x :) <$> unfoldrM f newSeed

initialState ::
  (Ord c) =>
  (a -> b -> c) ->
  NonEmpty a ->
  NonEmpty b ->
  Strict.ST s (c, Frontier s a b c)
initialState f as bs = do
  list <- DoublyLinked.empty
  rootPosition <- DoublyLinked.cons list (0 :: Int, 0 :: Int)
  let rootNode = mkNode f rootPosition as bs
  let emptyFrontier = Frontier MinPQueue.empty
  peekInsertChildren f rootNode emptyFrontier

step ::
  (Ord c) =>
  (a -> b -> c) ->
  Frontier s a b c ->
  MaybeT (Strict.ST s) (c, Frontier s a b c)
step f = deleteMinNode >=> lift . uncurry (peekInsertChildren f)

deleteMinNode ::
  (Ord c) => Frontier s a b c -> MaybeT (Strict.ST s) (Node s a b c, Frontier s a b c)
deleteMinNode frontier = do
  (node, queue') <- hoistMaybe (MinPQueue.minView (queue frontier))
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

-- Take a node not in the frontier but whose position is still in the position
-- list, add its children to the frontier, and remove the node from the
-- position list.
peekInsertChildren ::
  (Ord c) =>
  (a -> b -> c) ->
  Node s a b c ->
  Frontier s a b c ->
  Strict.ST s (c, Frontier s a b c)
peekInsertChildren f node frontier = do
  frontier' <-
    insertChildA f node frontier
      >>= insertChildB f node
  DoublyLinked.delete (position node)
  pure (value node, frontier')

insertChildA ::
  (Ord c) =>
  (a -> b -> c) ->
  Node s a b c ->
  Frontier s a b c ->
  Strict.ST s (Frontier s a b c)
insertChildA f node frontier = fmap (fromMaybe frontier) $ runMaybeT $ do
  let (ia, ib) = DoublyLinked.value (position node)
  nextPosition <- lift $ nextNodeValue (position node)
  guard (fmap fst nextPosition /= Just (ia + 1))
  as' <- hoistMaybe (nonEmpty (NonEmpty.tail (as node)))
  let bs' = bs node
  position' <- lift (DoublyLinked.insertAfter (position node) (ia + 1, ib))
  let value' = f (NonEmpty.head as') (NonEmpty.head bs')
  let node' =
        Node
          { position = position',
            value = value',
            as = as',
            bs = bs'
          }
  pure $ Frontier $ MinPQueue.insert value' node' (queue frontier)

insertChildB ::
  (Ord c) =>
  (a -> b -> c) ->
  Node s a b c ->
  Frontier s a b c ->
  Strict.ST s (Frontier s a b c)
insertChildB f node frontier = fmap (fromMaybe frontier) $ runMaybeT $ do
  let (ia, ib) = DoublyLinked.value (position node)
  prevPosition <- lift $ prevNodeValue (position node)
  guard (fmap snd prevPosition /= Just (ib + 1))
  bs' <- hoistMaybe (nonEmpty (NonEmpty.tail (bs node)))
  let as' = as node
  position' <- lift (DoublyLinked.insertBefore (position node) (ia, ib + 1))
  let value' = f (NonEmpty.head as') (NonEmpty.head bs')
  let node' = mkNode f position' as' bs'
  pure $ Frontier $ MinPQueue.insert value' node' (queue frontier)

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

-- Remove this once we allow transformers-0.6
hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure
