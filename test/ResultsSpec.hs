module ResultsSpec (spec) where

import Test.Hspec
import Results (prettyResults)

transformations = [
          (["This\tis a\nresult.\r"], "1. This\tis a\nresult.\r\n")
        , (["1", "2", "3", "4", "5"], "1. 1\n2. 2\n3. 3\n4. 4\n5. 5\n")
    ]

spec :: Spec
spec = do
    describe "Results module tests" $ do
        it "Does not transform an empty list of results" $
            prettyResults [] `shouldBe` Nothing
        it "Transforms list of results to an enumeration" $
            mapM_ shouldTransformTo $ transformations where
                shouldTransformTo (input, expectedOutput) =
                    prettyResults input `shouldBe` (Just expectedOutput)