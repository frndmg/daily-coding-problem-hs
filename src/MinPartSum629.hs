{-
This problem was asked by Etsy.

Given an array of numbers N and an integer k, your task is to split N into k
partitions such that the maximum sum of any partition is minimized. Return this
sum.

For example, given N = [5, 1, 2, 7, 3, 4] and k = 3, you should return 8, since
the optimal partition is [5, 1, 2], [7], [3, 4].
-}

module MinPartSum629 where

import qualified Data.Heap as Heap
import qualified Data.List as List

minPartSum :: [Int] -> Int -> Int
minPartSum xs k = minPartSum' partitionSums rest
  where
    minPartSum' partitionSums xs =
      case Heap.viewMin xs of
        Nothing -> foldl max 0 partitionSums
        Just (Heap.Entry _ x, xs') -> minPartSum' partitionSums' xs'
          where
            partitionSums' = Heap.adjustMin (+ x) partitionSums

    partitionSums = Heap.map Heap.payload greatestK
    (greatestK, rest) = Heap.splitAt k sortedXs
    sortedXs = Heap.fromList $ map (\x -> Heap.Entry (- x) x) xs
