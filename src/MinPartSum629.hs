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
