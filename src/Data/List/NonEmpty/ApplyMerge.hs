-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module: Data.List.NonEmpty.ApplyMerge
-- License: BSD-3-Clause
-- Maintainer: Preetham Gujjula <libraries@mail.preetham.io>
-- Stability: experimental
module Data.List.NonEmpty.ApplyMerge (applyMerge, applyMergeOn) where

import ApplyMerge.IntSet qualified
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup (Arg (..))

-- | Like 'Data.List.ApplyMerge.applyMerge', but operates on 'NonEmpty's instead
--   of lists.
applyMerge :: (Ord c) => (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
applyMerge = ApplyMerge.IntSet.applyMergeNonEmpty

-- | Like 'applyMerge', but applies a custom projection function before
--   performing comparisons.
applyMergeOn ::
  (Ord d) => (c -> d) -> (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
applyMergeOn p f as bs =
  let f' a b =
        let c = f a b
         in Arg (p c) c
   in NonEmpty.map (\(Arg _ c) -> c) (applyMerge f' as bs)
