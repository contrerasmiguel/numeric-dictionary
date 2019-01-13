module Results (prettyResults) where

lineElements lines = zip ['1'..] lines

mergedLineElements :: (Char, String) -> String
mergedLineElements (num, line) = num : ". " ++ line

linesWithFormat :: [String] -> [String]
linesWithFormat = (map mergedLineElements) . lineElements

prettyResults :: [String] -> Maybe String
prettyResults ls = case ls of
    [] -> Nothing
    _ -> Just $ unlines $ linesWithFormat ls