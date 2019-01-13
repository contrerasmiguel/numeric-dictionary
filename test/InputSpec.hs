module InputSpec (spec) where

import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Input (prettyInput)

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
shouldTransformTo (input, expectedOutput) =
    prettyInput input `shouldBe` expectedOutput

spec :: Spec
spec = do
    describe "Transformation tests" $ do
        it "Transforms an empty input to an empty list" $
            prettyInput emptyInput `shouldBe` emptyList

        it "Transforms raw input to a list of lower case lines" $
            mapM_ shouldTransformTo transformations
            
