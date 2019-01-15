module InputSpec (spec) where

import Input (prettyInput)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import TestUtil (fShouldTransformTo)

emptyInput :: String
emptyInput = ""

emptyList :: [String]
emptyList = []

transformations :: [(String, [String])]
transformations = [
      ("", [])
    , ("fIrst Line", ["first line"])
    , ("\r\tFIRST line\n", ["first line"])
    , ("FIRST line\n\n\n", ["first line"])
    , (" FIRST line\n\tsecond\tLines\n\t\t\rThird\t\tlines\t\n", ["first line"
        ,"second\tlines", "third\t\tlines"])
    ]

shouldTransformTo :: (String, [String]) -> Expectation
shouldTransformTo = fShouldTransformTo prettyInput

spec :: Spec
spec = do
    describe "Transformation tests" $ do
        it "Transforms raw input to a list of lower case lines" $
            mapM_ shouldTransformTo transformations
            
