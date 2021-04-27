module Overlapping637Spec where

import Overlapping637 (overlap)
import Test.Hspec
import Test.QuickCheck

-- TODO: Generate valid input data

allIntervalsAreOverlappedProp :: [(Int, Int)] -> Bool
allIntervalsAreOverlappedProp x = all isContained x
  where
    y = overlap x
    isContained x' = any (isContained' x') y
    isContained' (a, b) (a', b') = a' <= a && b <= b'

resultDoNotIntersectProp :: [(Int, Int)] -> Bool
resultDoNotIntersectProp x = not $ hasIntersection (overlap x)
  where
    hasIntersection is = or [hasIntersection' x' y' | x' <- is, y' <- is]
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
    -- it "overlap property" $ do
    --   property allIntervalsAreOverlappedProp
    -- it "no intersect property" $ do
    --   property resultDoNotIntersectProp
