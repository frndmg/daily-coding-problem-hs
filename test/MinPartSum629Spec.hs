module MinPartSum629Spec where

import Test.Hspec
import MinPartSum629 (minPartSum)

spec :: Spec
spec = do
    describe "MinPartSum" $ do
        it "works" $ do
            minPartSum [5, 1, 2, 7, 3, 4] 3 `shouldBe` 8
