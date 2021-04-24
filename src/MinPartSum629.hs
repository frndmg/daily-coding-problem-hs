module MinPartSum629 where

import qualified Data.Heap as Heap
import qualified Data.List as List

minPartSum :: [Int] -> Int -> Int
minPartSum xs k =
  minPartSum' (Heap.fromList firstK) rest
  where
    minPartSum' cumSums [] = foldl max 0 cumSums
    minPartSum' cumSums (x : xs) =
      minPartSum' cumSums' xs
      where
        cumSums' = Heap.adjustMin (+ x) cumSums

    firstK = take k xs'
    rest = drop k xs'
    xs' = List.sortOn negate xs
