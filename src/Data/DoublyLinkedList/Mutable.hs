-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Data.DoublyLinkedList.Mutable
  ( -- * Types
    List (..),
    FirstNode,
    ValueNode,
    LastNode,
    NextNode,
    PrevNode,

    -- * Node conversion
    nextNodeToValueNode,
    prevNodeToValueNode,

    -- * Construction
    empty,
    fromList,

    -- * Traversal

    -- ** Next node
    HasNext,
    next,

    -- ** Previous node
    HasPrev,
    prev,

    -- * Query
    null,
    value,

    -- * Insertion
    cons,
    snoc,
    insertBefore,
    insertAfter,

    -- * Deletion
    deleteNode,

    -- * List conversion
    toList,
  )
where

import Control.Arrow ((>>>))
import Control.Monad (forM_)
import Control.Monad.ST.Lazy (ST)
import Data.Maybe (isNothing)
import Data.STRef.Lazy (STRef, newSTRef, readSTRef, writeSTRef)
import Prelude hiding (null)

data List s a = List
  { firstNode :: FirstNode s a,
    lastNode :: LastNode s a
  }

-- | The sentinel node at the beginning of the list.
newtype FirstNode s a = FirstNode
  {nextRef :: STRef s (NextNode s a)}

-- | Nodes in the middle of the list that contain values.
data ValueNode s a = ValueNode
  { _value :: a,
    nextRef :: STRef s (NextNode s a),
    prevRef :: STRef s (PrevNode s a)
  }

-- | The sentinel node at the end of the list.
newtype LastNode s a = LastNode
  { prevRef :: STRef s (PrevNode s a)
  }

-- | Type for nodes that are after other nodes.
newtype NextNode s a = NextNode
  { unNextNode :: Either (ValueNode s a) (LastNode s a)
  }

nextNodeToValueNode :: NextNode s a -> Maybe (ValueNode s a)
nextNodeToValueNode (NextNode (Left v)) = Just v
nextNodeToValueNode _ = Nothing

-- | Type for nodes that are before other nodes.
newtype PrevNode s a = PrevNode
  { unPrevNode :: Either (FirstNode s a) (ValueNode s a)
  }

prevNodeToValueNode :: PrevNode s a -> Maybe (ValueNode s a)
prevNodeToValueNode (PrevNode (Right v)) = Just v
prevNodeToValueNode _ = Nothing

class HasNext node where
  next :: node s a -> ST s (NextNode s a)
  next = let x = x in x

  setNext :: (HasPrev next_node) => node s a -> next_node s a -> ST s ()
  setNext = let x = x in x

  toPrev :: node s a -> PrevNode s a
  toPrev = let x = x in x

  -- Abuse the MINIMAL pragma to not include hidden functions in Haddocks
  {-# MINIMAL #-}

instance HasNext FirstNode where
  next (FirstNode nextRef) = readSTRef nextRef
  {-# INLINE next #-}
  setNext (FirstNode nextRef) nextNode = writeSTRef nextRef (toNext nextNode)
  {-# INLINE setNext #-}
  toPrev = PrevNode . Left
  {-# INLINE toPrev #-}

instance HasNext ValueNode where
  next valueNode = readSTRef valueNode.nextRef
  {-# INLINE next #-}
  setNext valueNode nextNode = writeSTRef valueNode.nextRef (toNext nextNode)
  {-# INLINE setNext #-}
  toPrev = PrevNode . Right
  {-# INLINE toPrev #-}

instance HasNext PrevNode where
  next = either next next . unPrevNode
  {-# INLINE next #-}
  setNext prevNode nextNode =
    either (`setNext` nextNode) (`setNext` nextNode) (unPrevNode prevNode)
  {-# INLINE setNext #-}
  toPrev = id
  {-# INLINE toPrev #-}

class HasPrev node where
  prev :: node s a -> ST s (PrevNode s a)
  prev = let x = x in x

  setPrev :: (HasNext prev_node) => node s a -> prev_node s a -> ST s ()
  setPrev = let x = x in x

  toNext :: node s a -> NextNode s a
  toNext = let x = x in x

  -- Abuse the MINIMAL pragma to not include hidden functions in Haddocks
  {-# MINIMAL #-}

instance HasPrev ValueNode where
  prev valueNode = readSTRef valueNode.prevRef
  {-# INLINE prev #-}
  setPrev valueNode prevNode = writeSTRef valueNode.prevRef (toPrev prevNode)
  {-# INLINE setPrev #-}
  toNext = NextNode . Left
  {-# INLINE toNext #-}

instance HasPrev LastNode where
  prev (LastNode prevRef) = readSTRef prevRef
  {-# INLINE prev #-}
  setPrev (LastNode prevRef) prevNode = writeSTRef prevRef (toPrev prevNode)
  {-# INLINE setPrev #-}
  toNext = NextNode . Right
  {-# INLINE toNext #-}

instance HasPrev NextNode where
  prev = either prev prev . unNextNode
  {-# INLINE prev #-}
  setPrev nextNode prevNode =
    either (`setPrev` prevNode) (`setPrev` prevNode) (unNextNode nextNode)
  {-# INLINE setPrev #-}
  toNext = id
  {-# INLINE toNext #-}

null :: List s a -> ST s Bool
null list = isNothing . nextNodeToValueNode <$> next list.firstNode
{-# INLINE null #-}

value :: ValueNode s a -> a
value = _value
{-# INLINE value #-}

empty :: ST s (List s a)
empty = do
  firstNode <- FirstNode <$> newSTRef undefined
  lastNode <- LastNode <$> newSTRef undefined
  writeSTRef firstNode.nextRef (NextNode (Right lastNode))
  writeSTRef lastNode.prevRef (PrevNode (Left firstNode))
  pure (List firstNode lastNode)

insertAfter :: (HasNext node) => node s a -> a -> ST s (ValueNode s a)
insertAfter nodeA x = do
  nodeC <- next nodeA
  nodeB <- ValueNode x <$> newSTRef nodeC <*> newSTRef (toPrev nodeA)
  setNext nodeA nodeB
  setPrev nodeC nodeB
  pure nodeB

insertBefore :: (HasPrev node) => node s a -> a -> ST s (ValueNode s a)
insertBefore nodeC x = do
  nodeA <- prev nodeC
  nodeB <- ValueNode x <$> newSTRef (toNext nodeC) <*> newSTRef nodeA
  setNext nodeA nodeB
  setPrev nodeC nodeB
  pure nodeB

cons :: List s a -> a -> ST s (ValueNode s a)
cons list = insertAfter list.firstNode
{-# INLINE cons #-}

snoc :: List s a -> a -> ST s (ValueNode s a)
snoc list = insertBefore list.lastNode
{-# INLINE snoc #-}

deleteNode :: ValueNode s a -> ST s ()
deleteNode nodeB = do
  nodeA <- prev nodeB
  nodeC <- next nodeB
  setNext nodeA nodeC
  setPrev nodeC nodeA

fromList :: [a] -> ST s (List s a)
fromList xs = do
  list <- empty
  forM_ xs $ \x ->
    snoc list x
  pure list

toList :: List s a -> ST s [a]
toList list = next list.firstNode >>= go
  where
    go :: NextNode s a -> ST s [a]
    go =
      nextNodeToValueNode >>> \case
        Nothing -> pure []
        Just valueNode -> (valueNode._value :) <$> (next valueNode >>= go)
