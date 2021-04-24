module Area634Spec where

import Area634 (area)
import Test.Hspec

spec :: Spec
spec = do
  describe "Area" $ do
    it "computes the right area" $ do
      area [1, 3, 2, 5] `shouldBe` 6
