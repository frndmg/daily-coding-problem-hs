module Overlapping637 where

import Data.List (sortOn)

overlap :: [(Int, Int)] -> [(Int, Int)]
overlap [] = []
overlap x' = fst overlap' ++ [snd overlap']
  where
    (x:xs) = sortOn fst x'

    overlap' = foldl merge ([], x) xs

    merge (acc, y) x
      | shouldMerge y x = (acc, merge' y x)
      | otherwise = (acc ++ [y], x)

    shouldMerge (_, b) (a', _) = a' <= b
    merge' (a, b) (_, b') = (a, max b b')
