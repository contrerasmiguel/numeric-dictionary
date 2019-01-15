module NumericValue (readExpr) where

import Text.ParserCombinators.Parsec

type NumberValue = (Integer, String)

parsePair :: NumberValue -> Parser Integer
parsePair (result, expr) = try (string expr) >> pure result

multiplier :: Parser Integer
multiplier = choice $ map parsePair [
      (100, "hundred")
    , (1000, "thousand")
    , (1000000, "million")
    , (1000000000000, "billion")
    ]

multipleOfTen :: Parser Integer
multipleOfTen = choice $ map parsePair $ zip [20, 30..] [
      "twenty"
    , "thirty"
    , "fourty"
    , "fifty"
    , "sixty"
    , "seventy"
    , "eighty"
    , "ninety"
    ]

tenBelowTwenty :: Parser Integer
tenBelowTwenty = choice $ map parsePair $ zip [19, 18..] [
      "nineteen"
    , "eighteen"
    , "seventeen"
    , "sixteen"
    , "fifteen"
    , "fourteen"
    , "thirteen"
    , "twelve"
    , "eleven"
    , "ten"
    ]

belowTen :: Parser Integer
belowTen = choice $ map parsePair $ zip [9, 8..] [
      "nine"
    , "eight"
    , "seven"
    , "six"
    , "five"
    , "four"
    , "three"
    , "two"
    , "one"
    ]

zero :: Parser Integer
zero = parsePair (0, "zero")

twentyToNinetyNine :: Parser Integer
twentyToNinetyNine = try (do
    big <- multipleOfTen
    char '-'
    small <- belowTen
    pure $ big + small) <|> multipleOfTen

singleWordNumber :: Parser Integer
singleWordNumber = twentyToNinetyNine <|> tenBelowTwenty <|> belowTen

withMultiplier :: Parser Integer
withMultiplier = try (do
    small <- singleWordNumber
    skipMany1 space
    big <- multiplier
    pure $ small * big) <|> singleWordNumber

parseNumbers :: Parser Integer
parseNumbers =  zero <|> withMultiplier

parseExpr :: Parser Integer
parseExpr = parseNumbers

readExpr :: String -> String
readExpr input = case parse parseExpr "" input of
    Left e -> "No match: " ++ show e
    Right v -> show v