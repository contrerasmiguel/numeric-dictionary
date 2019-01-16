module NumericValue (readExpr) where

import Text.ParserCombinators.Parsec (
      Parser
    , char
    , choice
    , parse
    , skipMany1
    , space
    , string
    , try
    , (<|>))

type NumberValue = (Integer, String)

parsePair :: NumberValue -> Parser Integer
parsePair (result, expr) = try (string expr) >> pure result

multipleOf10 :: Parser Integer
multipleOf10 = choice $ map parsePair $ zip [20, 30..] [
      "twenty"
    , "thirty"
    , "fourty"
    , "fifty"
    , "sixty"
    , "seventy"
    , "eighty"
    , "ninety"]

tenTo19 :: Parser Integer
tenTo19 = choice $ map parsePair $ zip [19, 18..] [
      "nineteen"
    , "eighteen"
    , "seventeen"
    , "sixteen"
    , "fifteen"
    , "fourteen"
    , "thirteen"
    , "twelve"
    , "eleven"
    , "ten"]

oneTo9 :: Parser Integer
oneTo9 = choice $ map parsePair $ zip [9, 8..] [
      "nine"
    , "eight"
    , "seven"
    , "six"
    , "five"
    , "four"
    , "three"
    , "two"
    , "one"]

opBySep :: Parser a -> Parser b -> (b -> b -> b) -> Parser b -> Parser b
opBySep separator big op small = try (do
    b <- big
    separator
    s <- small
    pure $ b `op` s) <|> big

opBySpaces :: Parser b -> (b -> b -> b) -> Parser b -> Parser b
opBySpaces = opBySep $ skipMany1 space

twentyTo99 :: Parser Integer
twentyTo99 = opBySep (char '-') multipleOf10 (+) oneTo9

multipliers :: [Parser Integer]
multipliers = map parsePair [
      (1000000000000, "billion")
    , (1000000, "million")
    , (1000, "thousand")
    , (100, "hundred")]

oneTo99 :: Parser Integer
oneTo99 = twentyTo99 <|> tenTo19 <|> oneTo9

oneToN :: Parser Integer -> Parser Integer -> Parser Integer
oneToN multiplier previousOneToN = opBySpaces multiplesOfM (+) previousOneToN
    where multiplesOfM = opBySpaces previousOneToN (*) multiplier

oneToBillions :: Parser Integer
oneToBillions = foldr oneToN oneTo99 multipliers

negative :: Parser Integer
negative = parsePair (-1, "negative")

signedOneToBillions :: Parser Integer
signedOneToBillions = try (do
    sign <- negative
    skipMany1 space
    number <- oneToBillions
    pure $ sign * number) <|> oneToBillions

zero :: Parser Integer
zero = parsePair (0, "zero")

parseExpr :: Parser Integer
parseExpr = zero <|> signedOneToBillions

readExpr :: String -> String
readExpr input = case parse parseExpr "" input of
    Left e -> "No match: " ++ show e
    Right v -> show v