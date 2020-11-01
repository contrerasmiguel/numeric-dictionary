module TestUtil (fShouldTransformTo) where

import Test.Hspec (Expectation, shouldBe)

fShouldTransformTo :: (Show b, Eq b) => (a -> b) -> (a, b) -> Expectation
fShouldTransformTo f (input, expectedOutput) =
    f input `shouldBe` expectedOutput