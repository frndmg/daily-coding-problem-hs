{-# LANGUAGE ParallelListComp #-}

module Area634 where

-- This problem was asked by Square.

-- You are given a histogram consisting of rectangles of different heights.
-- These heights are represented in an input list, such that [1, 3, 2, 5]
-- corresponds to the following diagram:
--       x
--       x
--   x   x
--   x x x
-- x x x x
-- Determine the area of the largest rectangle that can be formed only from the
-- bars of the histogram. For the diagram above, for example, this would be six,
-- representing the 2 x 3 area at the bottom right.

-- |Compute the maximum area of the figure given by the heights of the columns
area :: [Int] -> Int
area colHeights = area' [] colHeights 0
  where
    area' _ [] maxArea = maxArea
    area' prevAccumulatedAreas (colHeight : restColHeights) maxArea =
      area' nextAccumulatedAreas restColHeights (max maxArea maxArea')
      where
        maxArea' = foldl max 0 nextAccumulatedAreas
        nextAccumulatedAreas =
          [ height + accumulatedArea
            | height <- [1 .. colHeight]
            | accumulatedArea <- prevAccumulatedAreas ++ repeat 0
          ]
