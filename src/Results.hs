module Results (prettyResults) where

lineElements :: [a] -> [(Char, a)]
lineElements = zip ['1'..]

mergedLineElements :: (Char, String) -> String
mergedLineElements (num, line) = num : ". " ++ line

linesWithFormat :: [String] -> [String]
linesWithFormat = map mergedLineElements . lineElements

prettyResults :: [String] -> String
prettyResults = unlines . linesWithFormat