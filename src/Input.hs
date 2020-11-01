module Input (prettyInput) where

import Data.Char (isSpace, toLower)
import Data.List (dropWhile, dropWhileEnd)

takeNotEmpty :: [String] -> [String]
takeNotEmpty = filter (not . null)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

allToLower :: String -> String
allToLower = map toLower

prettyInput :: String -> [String]
prettyInput = takeNotEmpty . (map $ trim . allToLower) . lines