module Main (main) where

import           Data.Function       (on)
import           Data.List.Duplicate (deleteAdjDupsBy)
import           Data.List.Filter    (takeEvery)
import           Data.List.Merge     (applyMerge)
import           Data.List.Ordered   (isectBy)

squares :: [Int]
squares = map (\x -> x * x) [1..]

fifthPowers :: [Int]
fifthPowers = map (^5) [1..]

singletons :: [(Int, [Int])]
singletons = map (\x -> (x, [x])) fifthPowers

add :: (Int, [Int]) -> (Int, [Int]) -> (Int, [Int])
add (x, xs) (y, ys) = (x + y, xs ++ ys)

pairs :: [(Int, [Int])]
pairs = deleteAdjDupsBy ((==) `on` fst)
      $ applyMerge add singletons singletons

quads :: [(Int, [Int])]
quads = deleteAdjDupsBy ((==) `on` fst)
      $ applyMerge add pairs pairs

main :: IO ()
main = mapM_ print $ isectBy (compare `on` fst) quads singletons
