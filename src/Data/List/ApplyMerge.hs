-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Data.List.ApplyMerge
-- License: BSD-3-Clause
-- Maintainer: Preetham Gujjula <libraries@mail.preetham.io>
-- Stability: experimental
--
-- Lift a binary, non-decreasing function onto ordered lists and order the
-- output.
--
-- @since 0.1.0.0
module Data.List.ApplyMerge
  ( -- * Functions
    applyMerge,
    applyMergeBy,
    applyMergeOn,

    -- * Ascii drawing
    drawMergePattern,
  )
where

import qualified ApplyMerge.IntSet
import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Function ((&))
import Data.Proxy (Proxy (..))
import Data.Reflection (Reifies, reflect, reify)
import Data.Semigroup (Arg (..))
import Data.Text (Text)
import qualified Data.Text as Text

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
--
--   @since 0.1.0.0
applyMerge :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge = ApplyMerge.IntSet.applyMerge

-- | Like 'applyMerge', but uses a custom comparison function.
--
--   @since 0.1.1.0
applyMergeBy :: (c -> c -> Ordering) -> (a -> b -> c) -> [a] -> [b] -> [c]
applyMergeBy = applyMergeBy_

-- Reflection logic in applyMerge_ is based on "All about reflection: a
-- tutorial" [1] by Arnaud Spiwack, licensed under CC BY 4.0 [2].
--
-- [1]: https://www.tweag.io/blog/2017-12-21-reflection-tutorial/
-- [2]: https://creativecommons.org/licenses/by/4.0/
applyMergeBy_ ::
  forall a b c. (c -> c -> Ordering) -> (a -> b -> c) -> [a] -> [b] -> [c]
applyMergeBy_ cmp f as bs =
  reify cmp $ \(_ :: Proxy s) ->
    let f' :: a -> b -> ReflectedOrd s c
        f' a b = ReflectedOrd (f a b)
     in map unReflectedOrd (applyMerge f' as bs)

newtype ReflectedOrd s a = ReflectedOrd {unReflectedOrd :: a}

instance (Reifies s (a -> a -> Ordering)) => Eq (ReflectedOrd s a) where
  (==) (ReflectedOrd x) (ReflectedOrd y) =
    let cmp = reflect (Proxy :: Proxy s)
     in cmp x y == EQ

instance (Reifies s (a -> a -> Ordering)) => Ord (ReflectedOrd s a) where
  compare (ReflectedOrd x) (ReflectedOrd y) =
    let cmp = reflect (Proxy :: Proxy s)
     in cmp x y

-- | Like 'applyMerge', but applies a custom projection function before
--   performing comparisons.
--
--   For example, to compute the Gaussian integers, ordered by norm:
--
--   > zs :: [Integer]
--   > zs = 0 : concatMap (\i -> [i, -i]) [1..]
--   >
--   > gaussianIntegers :: [GaussianInteger]      -- `GaussianInteger` from arithmoi
--   > gaussianIntegers = applyMergeOn norm (:+) zs zs
--
--   @since 0.1.1.0
applyMergeOn ::
  (Ord d) => (c -> d) -> (a -> b -> c) -> [a] -> [b] -> [c]
applyMergeOn p f as bs =
  let f' a b =
        let c = f a b
         in Arg (p c) c
   in map (\(Arg _ c) -> c) (applyMerge f' as bs)

type ApplyMerge = forall a b c. (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]

-- | @drawMergePattern f as bs size n@ takes the parameters for an @applyMerge@
--   and displays what the smallest @size@ x @size@ elements look like after @n@
--   elements have been generated.
--
-- >>> import Data.Text.IO qualified as Text
-- >>> Text.putStr (drawMergePattern (+) [1..] [1..] 10 50)
-- . . . . . . . . . *
-- . . . . . . . . * *
-- . . . . . . . * * *
-- . . . . . . * * * *
-- . . . . . * * * * *
-- . . . . . * * * * *
-- . . . . * * * * * *
-- . . . * * * * * * *
-- . . * * * * * * * *
-- . * * * * * * * * *
--
--   @since 0.1.2.0
drawMergePattern :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> Int -> Int -> Text
drawMergePattern = drawMergePatternWith applyMerge

drawMergePatternWith ::
  forall a b c.
  (Ord c) =>
  ApplyMerge ->
  (a -> b -> c) ->
  [a] ->
  [b] ->
  Int ->
  Int ->
  Text
drawMergePatternWith applyMerge' f as bs size n =
  let labeledAs :: [(a, Int)]
      labeledAs = zip as [0 ..]

      labeledBs :: [(b, Int)]
      labeledBs = zip bs [0 ..]

      f' :: (a, Int) -> (b, Int) -> (c, (Int, Int))
      f' (a, x) (b, y) = (f a b, (x, y))

      labeledCs :: [(c, (Int, Int))]
      labeledCs = applyMerge' f' labeledAs labeledBs

      markedIndices :: [(Int, Int)]
      markedIndices =
        labeledCs
          & take n
          & map snd
          & filter (\(x, y) -> x < size && y < size)

      markedArray :: Array (Int, Int) Bool
      markedArray =
        let r = ((0, 0), (size - 1, size - 1))
         in Array.array ((0, 0), (size - 1, size - 1)) $
              map (,False) (Array.range r)
                ++ map (,True) markedIndices

      mkRow :: Int -> Text
      mkRow row =
        Text.intersperse ' ' $
          Text.pack $
            flip map [0 .. size - 1] $ \i ->
              if markedArray ! (row, i)
                then '.'
                else '*'
   in Text.unlines (map mkRow [0 .. size - 1])
