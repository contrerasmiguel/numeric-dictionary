module NumericValueSpec (spec) where

import NumericValue (readExpr)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import TestUtil (fShouldTransformTo)

transformations :: [(String, String)]
transformations = [
      ("bla bla bla", "Could not parse this line.")
    , ("zero", "0")
    , ("one", "1")
    , ("one hundred", "100")
    , ("one hundred ninety-eight", "198")
    , ("two", "2")
    , ("three", "3")
    , ("four", "4")
    , ("five", "5")
    , ("six",  "6")
    , ("seven", "7")
    , ("eight", "8")
    , ("nine", "9")
    , ("nine thousand", "9000")
    , ("ten", "10")
    , ("ten thousand", "10000")
    , ("eleven", "11")
    , ("twelve", "12")
    , ("thirteen", "13")
    , ("fourteen", "14")
    , ("fifteen", "15")
    , ("sixteen", "16")
    , ("seventeen", "17")
    , ("eighteen", "18")
    , ("nineteen", "19")
    , ("nineteen hundred", "1900")
    , ("twenty", "20")
    , ("twenty-one", "21")
    , ("twenty-one hundred", "2100")
    , ("thirty", "30")
    , ("thirty-two", "32")
    , ("thirty-two thousand", "32000")
    , ("fourty", "40")
    , ("fifty", "50")
    , ("sixty", "60")
    , ("seventy", "70")
    , ("eighty", "80")
    , ("eighty-eight", "88")
    , ("eighty-eight thousand", "88000")
    , ("ninety", "90")
    , ("ninety-nine", "99")
    , ("ninety-nine hundred", "9900")
    , ("nine thousand nine hundred ninety-nine", "9999")
    , ("ninety-nine hundred ninety-nine thousand nine hundred ninety-nine", "9999999")
    , ("five million", "5000000")
    , ("ninety-nine hundred ninety-nine thousand nine hundred ninety-nine million nine hundred ninety-nine thousand nine hundred ninety-nine", "9999999999999")
    , ("four billion", "4000000000000")
    , ("ninety-nine hundred ninety-nine thousand nine hundred ninety-nine million nine hundred ninety-nine thousand nine hundred ninety-nine billion nine hundred ninety-nine thousand nine hundred ninety-nine million nine hundred ninety-nine thousand nine hundred ninety-nine", "9999999999999999999999999")
    , ("negative ninety-nine hundred ninety-nine thousand nine hundred ninety-nine million nine hundred ninety-nine thousand nine hundred ninety-nine billion nine hundred ninety-nine thousand nine hundred ninety-nine million nine hundred ninety-nine thousand nine hundred ninety-nine", "-9999999999999999999999999")]

shouldParseExpression :: (String, String) -> Expectation
shouldParseExpression = fShouldTransformTo readExpr

spec :: Spec
spec = do
    describe "Transformation tests" $ do
        it "Transforms an expression to its corresponding numeric value" $
            mapM_ shouldParseExpression transformations
        