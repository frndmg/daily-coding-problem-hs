module Overlapping637Spec where

import Data.List
import Overlapping637
import Test.Hspec
import Test.QuickCheck

instance Arbitrary Interval where
  arbitrary = do
    -- TODO: How to not bound the numbers?
    -- TODO: How to create a reduction strategy?
    x <- choose (-10000, 10000)
    y <- choose (x, 100000)
    return (Interval x y)

-- !give all pairs in an array
pairs :: Eq a => [a] -> [(a, a)]
pairs xs = [(x, y) | (x : ys) <- tails (nub xs), y <- ys]

allIntervalsAreOverlappedProp :: [Interval] -> Bool
allIntervalsAreOverlappedProp is = all (contained is') is
  where
    is' = overlap is
    contained is i = any (contains i) is
    contains (Interval a b) (Interval a' b') = a' <= a && b <= b'

resultDoNotIntersectProp :: [Interval] -> Bool
resultDoNotIntersectProp is =
  and [not $ areOverlapping x' y' | (x', y') <- pairs is']
  where
    is' = overlap is

validIntervalsProp :: [Interval] -> Bool
validIntervalsProp is = all isValidInterval (overlap is)

spec :: Spec
spec = do
  describe "Overlapping" $ do
    it "should tell you if intervals overlap" $ do
      areOverlapping (Interval 5 8) (Interval 4 10) `shouldBe` True

    it "should merge overlapping intervals" $ do
      merge (Interval 5 8) (Interval 4 10) `shouldBe` Interval 4 10

    it "should compute overlapping" $
      do
        overlap (map fromTuple [(1, 3), (5, 8), (4, 10), (20, 25)])
          `shouldBe` map fromTuple [(1, 3), (4, 10), (20, 25)]

    it "should cover the original intervals" $ do
      property allIntervalsAreOverlappedProp

    it "should not overlap each other" $ do
      property resultDoNotIntersectProp

    it "should create valid intervals" $ do
      property validIntervalsProp
