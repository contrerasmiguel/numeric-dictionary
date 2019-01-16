module NumericValue (readExpr) where

import Text.ParserCombinators.Parsec

type NumberValue = (Integer, String)

parsePair :: NumberValue -> Parser Integer
parsePair (result, expr) = try (string expr) >> pure result

-- multiplier :: Parser Integer
-- multiplier = choice $ map parsePair [
--       (100, "hundred")
--     , (1000, "thousand")
--     , (1000000, "million")
--     , (1000000000000, "billion")
--     ]

billion :: Parser Integer
billion = parsePair (1000000000000, "billion")

million :: Parser Integer
million = parsePair (1000000, "million")

thousand :: Parser Integer
thousand = parsePair (1000, "thousand")

hundred :: Parser Integer
hundred = parsePair (100, "hundred")

multipleOf10 :: Parser Integer
multipleOf10 = choice $ map parsePair $ zip [20, 30..] [
      "twenty"
    , "thirty"
    , "fourty"
    , "fifty"
    , "sixty"
    , "seventy"
    , "eighty"
    , "ninety"
    ]

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
    , "ten"
    ]

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
    , "one"
    ]

zero :: Parser Integer
zero = parsePair (0, "zero")

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

oneTo99 :: Parser Integer
oneTo99 = twentyTo99 <|> tenTo19 <|> oneTo9

multiplesOf100 :: Parser Integer
multiplesOf100 = opBySpaces oneTo99 (*) hundred
    
oneTo9999 :: Parser Integer
oneTo9999 = opBySpaces multiplesOf100 (+) oneTo99

multiplesOf1000 :: Parser Integer
multiplesOf1000 = opBySpaces oneTo9999 (*) thousand

oneTo9999999 :: Parser Integer
oneTo9999999 = opBySpaces multiplesOf1000 (+) oneTo9999

multiplesOf1000000 :: Parser Integer
multiplesOf1000000 = opBySpaces oneTo9999999 (*) million

oneTo9999999999 :: Parser Integer
oneTo9999999999 = opBySpaces multiplesOf1000000 (+) oneTo9999999

multiplesOf1000000000000 :: Parser Integer
multiplesOf1000000000000 = opBySpaces oneTo9999999999 (*) billion

oneTo9999999999999999999999999 :: Parser Integer
oneTo9999999999999999999999999 = opBySpaces multiplesOf1000000000000 (+) oneTo9999999999

parseNumbers :: Parser Integer
parseNumbers =  zero <|> oneTo9999999999999999999999999

parseExpr :: Parser Integer
parseExpr = parseNumbers

readExpr :: String -> String
readExpr input = case parse parseExpr "" input of
    Left e -> "No match: " ++ show e
    Right v -> show v