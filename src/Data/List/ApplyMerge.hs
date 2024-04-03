-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Data.List.ApplyMerge (applyMerge) where

import Data.List.ApplyMerge.IntSet qualified

applyMerge :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge = Data.List.ApplyMerge.IntSet.applyMerge
