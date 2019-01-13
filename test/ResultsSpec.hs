module ResultsSpec (spec) where

import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Results (prettyResults)

transformations :: [([String], String)]
transformations = [
          ([], "")
        , (["This\tis a\nresult.\r"], "1. This\tis a\nresult.\r\n")
        , (["1", "2", "3", "4", "5"], "1. 1\n2. 2\n3. 3\n4. 4\n5. 5\n")
    ]

shouldTransformTo :: ([String], String) -> Expectation
shouldTransformTo (input, expectedOutput) =
    prettyResults input `shouldBe` expectedOutput

spec :: Spec
spec = do
    describe "Transformation tests" $ do
        it "Transforms list of results to an enumeration" $
            mapM_ shouldTransformTo transformations
                