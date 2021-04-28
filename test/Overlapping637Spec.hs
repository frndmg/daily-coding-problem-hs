module Overlapping637Spec where

import Overlapping637 (overlap)
import Test.Hspec
import Test.QuickCheck
import Data.List

monotonicTuple' :: Gen (Int, Int)
monotonicTuple' = do
  -- TODO: How to not bound the numbers?
  x <- choose (-10000, 10000)
  y <- choose (x, 100000)
  return (x, y)

monotonicTuples :: Gen [(Int, Int)]
monotonicTuples =
  -- TODO: How to generate a list with my generator?
  sized $
    \n -> do
      k <- choose (0, n)
      sequence [monotonicTuple' | _ <- [1..k]]

allIntervalsAreOverlappedProp :: [(Int, Int)] -> Bool
allIntervalsAreOverlappedProp x = all isContained x
  where
    y = overlap x
    isContained x' = any (isContained' x') y
    isContained' (a, b) (a', b') = a' <= a && b <= b'

pairs xs = [(x, y) | (x:ys) <- tails (nub xs), y <- ys]

resultDoNotIntersectProp :: [(Int, Int)] -> Bool
resultDoNotIntersectProp x = not $ hasIntersection (overlap x)
  where
    hasIntersection is = or [hasIntersection' x' y' | (x', y') <- pairs is]
    hasIntersection' (a, b) (a', b') =
      a <= a' && a' <= b
        || a' <= a && a <= b'

spec :: Spec
spec = do
  describe "Overlapping" $ do
    it "compute overlapping" $
      do
        overlap [(1, 3), (5, 8), (4, 10), (20, 25)]
          `shouldBe` [(1, 3), (4, 10), (20, 25)]

    it "overlap property" $ do
      quickCheck (forAll monotonicTuples allIntervalsAreOverlappedProp)

    it "no intersect property" $ do
      quickCheck (forAll monotonicTuples resultDoNotIntersectProp)
