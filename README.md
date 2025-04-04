<!--
SPDX-FileCopyrightText: Copyright Preetham Gujjula
SPDX-License-Identifier: BSD-3-Clause
-->

# apply-merge

[![Made in Haskell](https://img.shields.io/badge/Made_in-Haskell-5e5086?logo=haskell&style=flat)](https://haskell.org)
[![CI](https://github.com/pgujjula/apply-merge/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/pgujjula/apply-merge/actions/workflows/ci.yml?query=branch%3Amain)
[![Hackage Version](https://img.shields.io/hackage/v/apply-merge?style=flat&color=blue)](https://hackage.haskell.org/package/apply-merge)
[![Stackage Nightly Version](http://stackage.org/package/apply-merge/badge/nightly?color=blue)](https://www.stackage.org/nightly/package/apply-merge)
[![Stackage LTS Version](http://stackage.org/package/apply-merge/badge/lts?color=blue)](https://www.stackage.org/lts/package/apply-merge)

Lift a binary, non-decreasing function onto ordered lists and order the output

## External Discussions

* Reddit thread: [link](https://www.reddit.com/r/haskell/comments/1bwpe33/applymerge_lift_a_binary_nondecreasing_function/)
* Haskell Discourse thread: [link](https://discourse.haskell.org/t/apply-merge-lift-a-binary-increasing-function-onto-ordered-lists-and-produce-ordered-output/9269)

## Overview

This library provides a function

```haskell
applyMerge :: Ord c => (a -> b -> c) -> [a] -> [b] -> [c]
```

If `f` is a binary function that is non-decreasing in both arguments, and `xs`
and `ys` are (potentially infinite) ordered lists, then `applyMerge f xs ys` is
an ordered list of all `f x y`, for each `x` in `xs` and `y` in `ys`.

Producing $n$ elements of `applyMerge f xs ys` takes $O(n \log n)$ time and
$O(\sqrt{n})$ auxiliary space, assuming that `f` and `compare` take $O(1)$ time.
See <!-- Relative link doesn't work here for some reason, see issue #17 -->
[docs/ALGORITHM.md#note-about-memory-usage](https://github.com/pgujjula/apply-merge/blob/main/docs/ALGORITHM.md#note-about-memory-usage)
for caveats.

## Examples

With `applyMerge`, we can implement a variety of complex algorithms succinctly.
For example, the Sieve of Erastosthenes[^1] to generate prime numbers:

```haskell
primes :: [Int]
primes = 2 : ([3..] `minus` composites)    -- `minus` from data-ordlist

composites :: [Int]
composites = applyMerge (*) primes [2..]
```

3-smooth numbers ([Wikipedia](https://en.wikipedia.org/wiki/Smooth_number)):

```haskell
smooth3 :: [Integer]
smooth3 = applyMerge (*) (iterate (*2) 1) (iterate (*3) 1)
```

Gaussian integers, ordered by norm ([Wikipedia](https://en.wikipedia.org/wiki/Gaussian_integer)):

```haskell
zs :: [Integer]
zs = 0 : concatMap (\i -> [i, -i]) [1..]

gaussianIntegers :: [GaussianInteger]      -- `GaussianInteger` from arithmoi
gaussianIntegers = map snd (applyMerge (\x y -> (norm (x :+ y), x :+ y)) zs zs)
```

Square-free integers ([Wikipedia](https://en.wikipedia.org/wiki/Square-free_integer)):

```haskell
squarefrees :: [Int]
squarefrees = [1..] `minus` applyMerge (*) (map (^2) primes) [1..]
```

## Naming

The name `applyMerge` comes from the idea of applying `f` to each `x` and `y`,
and merging the results into one sorted output. I'm still thinking of the ideal
name for this function. Other options include `sortedLiftA2`/`orderedLiftA2`,
from the idea that this function is equivalent to `sort (liftA2 f xs ys)` when
`xs` and `ys` are finite. If you have any ideas on the naming, let me know!

## Further reading

See [docs/ALGORITHM.md](docs/ALGORITHM.md) for a full exposition of the
`applyMerge` function and its implementation.

## Licensing

[![REUSE status](https://api.reuse.software/badge/github.com/pgujjula/apply-merge)](https://api.reuse.software/info/github.com/pgujjula/apply-merge)

This project licensed under BSD-3-Clause (except for `.gitignore`, which is
under CC0-1.0), and follows [REUSE](https://reuse.software) licensing
principles.

[^1]: Note that this is really the Sieve of Erastosthenes, as defined in the classic [The Genuine Sieve of Eratosthenes](https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf). Constrast this to other simple prime generation implementations, such as <pre> primes = sieve [2..] where sieve (p : xs) = p : sieve [x | x <- xs, x \`rem\` p > 0]</pre> which is actually trial division and not a faithful implementation of the Sieve of Erastosthenes.
