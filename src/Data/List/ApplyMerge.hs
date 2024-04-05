-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module: Data.List.ApplyMerge
-- License: BSD-3-Clause
-- Maintainer: Preetham Gujjula <libraries@mail.preetham.io>
-- Stability: experimental
module Data.List.ApplyMerge (applyMerge) where

import Data.List.ApplyMerge.IntSet qualified

-- | If given a binary function @f@ that is non-decreasing in both arguments,
--   and two (potentially infinite) ordered lists @xs@ and @ys@, then
--   @'applyMerge' f xs ys@ is a sorted list of all @f x y@, for each @x@ in
--   @xs@ and @y@ in @ys@.
--
--   Producing \(n\) elements of @'applyMerge' f xs ys@ takes \(O(n \log n)\)
--   time and \(O(\sqrt{n})\) auxiliary space, assuming that @f@ and @compare@
--   take \(O(1)\) time.
--
--   For example, to generate the 3-smooth numbers
--   ([Wikipedia](https://en.wikipedia.org/wiki/Smooth_number)):
--
--   > smooth3 :: [Integer]
--   > smooth3 = applyMerge (*) (iterate (*2) 1) (iterate (*3) 1)
--
--   For more examples, see
--   [README#examples](https://github.com/pgujjula/apply-merge/#examples).
applyMerge :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge = Data.List.ApplyMerge.IntSet.applyMerge
