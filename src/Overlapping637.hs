module Overlapping637 where

import Data.List (sortOn)

data Interval = Interval
  { begin :: Int,
    end :: Int
  }
  deriving (Show, Eq)

fromTuple :: (Int, Int) -> Interval
fromTuple (a, b) = Interval a b

isValidInterval :: Interval -> Bool
isValidInterval (Interval a b) = a <= b

areOverlapping :: Interval -> Interval -> Bool
areOverlapping (Interval a b) (Interval a' b') =
  a <= a' && a' <= b
    || a' <= a && a <= b'

merge :: Interval -> Interval -> Interval
merge (Interval a b) (Interval a' b') = Interval (min a a') (max b b')

overlap :: [Interval] -> [Interval]
overlap [] = []
overlap is = fst overlapped ++ [snd overlapped]
  where
    (initialInterval : restOfIntervals) = sortOn begin is

    overlapped = foldl op ([], initialInterval) restOfIntervals

    op (acc, intervalCandidate) interval
      | areOverlapping intervalCandidate interval =
        (acc, merge intervalCandidate interval)
      | otherwise =
        (acc ++ [intervalCandidate], interval)
