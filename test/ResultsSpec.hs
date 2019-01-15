module ResultsSpec (spec) where

import Results (prettyResults)
import Test.Hspec (Expectation, Spec, describe, it)
import TestUtil (fShouldTransformTo)

transformations :: [([String], String)]
transformations = [
        ([], "")
    , (["This\tis a\nresult.\r"], "1. This\tis a\nresult.\r\n")
    , (["1", "2", "3", "4", "5"], "1. 1\n2. 2\n3. 3\n4. 4\n5. 5\n")
    ]

shouldTransformTo :: ([String], String) -> Expectation
shouldTransformTo = fShouldTransformTo prettyResults

spec :: Spec
spec = do
    describe "Transformation tests" $ do
        it "Transforms list of results to an enumeration" $
            mapM_ shouldTransformTo transformations
                