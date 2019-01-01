{-|
    Numeric Dictionary
    ====================

    Author: Miguel Contreras

    Description
    -----------
    Lee el contenido de un archivo de texto compuesto por líneas de texto. Cada 
    linea representa un número en inglés escrito de forma textual.
        Por ejemplo: "ninety eight"
    
    This application reads a text file that has lines that represent numbers
    written in English.

        For example: "negative ninety eight"

    Each line is transformed to a corresponding numeric representation and
    then they're stored in another text file.

    Usage:
        GNU/Linux: ./app_name input_file output_file
            Example: ./app ING.IN ING.OUT

        Windows: app_name input_file output_file
            Example: app.exe file.txt AnotherFile.out
 -}

import Control.Applicative
import Data.Char           (toLower)
import Data.Maybe          (isNothing)
import System.Environment  (getArgs)

units = ["one"
        ,"two"
        ,"three"
        ,"four"
        ,"five"
        ,"six"
        ,"seven"
        ,"eight"
        ,"nine"]

belowTwenty = ["ten"
              ,"eleven"
              ,"twelve"
              ,"thirteen"
              ,"fourteen"
              ,"fifteen"
              ,"sixteen"
              ,"seventeen"
              ,"eighteen"
              ,"nineteen"]

aboveTwenty = ["twenty"
              ,"thirty"
              ,"forty"
              ,"fifty"
              ,"sixty"
              ,"seventy"
              ,"eighty"
              ,"ninety"]

hundred = ["hundred"]

multipliers = ["thousand"
              ,"million"]

type Operator = Int -> Int

data NumberWord =
      Invalid String
    | Zero String
    | Unit String
    | BelowTwenty String
    | AboveTwenty String
    | Hundred String
    | Multiplier String
    deriving (Eq,Show)

data NumberWordOperator = NumberWordOperator NumberWord (Maybe Operator)

data MultiplierGroup = MultiplierGroup [NumberWordOperator] (Maybe NumberWordOperator)

class ReadableWord a where
    getOperator :: a -> Maybe (Operator)
    valid :: a -> Bool
    valid rw = case getOperator rw of
        Just _ -> True
        _      -> False

instance ReadableWord NumberWord where
    getOperator nw = case nw of
        Invalid _           -> Nothing

        Zero "zero"         -> Just (\_ -> 0)
        Zero _              -> Nothing

        Unit s              -> lookForValue s (+) units [1..]
        BelowTwenty s       -> lookForValue s (+) belowTwenty [10..]
        AboveTwenty s       -> lookForValue s (+) aboveTwenty [20,30..]
        Hundred s           -> lookForValue s (*) hundred [100]
        Multiplier s        -> lookForValue s (*) multipliers [1000,1000000..]

main = do
    args <- getArgs
    case args of
         (inputF:outputF:_) -> processFile f inputF outputF
         _                  -> putStrLn "You must specify an input file and an output file."
    where
        f content = unlines . addLineNumbers $ map formatParsedLines $ map parseLine $ map toLowerC $ lines content
        formatParsedLines (Just n) = show n
        formatParsedLines Nothing  = "Could not read line due to syntax errors on it."
        addLineNumbers ls = map p $ zip ['1'..] ls
        p (lineNumber, line) = lineNumber : ". " ++ line
        toLowerC = map toLower

processFile :: (String -> String) -> FilePath -> FilePath -> IO ()
processFile f inputFile outputFile = do
    content <- readFile inputFile
    putStrLn $ f content
    writeFile outputFile $ f content

parseLine :: String -> Maybe Int
parseLine line =
    let (sign,input) = case theWords of
                        ("negative":ws) -> ((*)(-1),ws)
                        _               -> (id,theWords)
    in
        sign <$> (pipeLine input)
    where
        theWords = words line
        pipeLine = matchMultiplierPattern . groupByMultipliers . parseWords

matchMultiplierPattern :: [MultiplierGroup] -> Maybe Int
matchMultiplierPattern mults =
    case mults of
        ((MultiplierGroup nwOps1 Nothing):[])                                                 -> matchAdderPattern nwOps1
        ((MultiplierGroup nwOps1 (Just (NumberWordOperator (Multiplier "thousand") op1))):[]) -> op1 <*> matchAdderPattern nwOps1
        ((MultiplierGroup nwOps1 (Just (NumberWordOperator (Multiplier "million") op1))):[])  -> op1 <*> matchAdderPattern nwOps1

        ((MultiplierGroup nwOps1 (Just (NumberWordOperator (Multiplier "thousand") op1))):(MultiplierGroup nwOps2 Nothing):[])                                                -> (+) <$> matchAdderPattern nwOps2 <*> (op1 <*> matchAdderPattern nwOps1)
        ((MultiplierGroup nwOps1 (Just (NumberWordOperator (Multiplier "million") op1))):(MultiplierGroup nwOps2 (Just (NumberWordOperator (Multiplier "thousand") op2))):[]) -> (+) <$> (op2 <*> matchAdderPattern nwOps2) <*> (op1 <*> matchAdderPattern nwOps1)
        ((MultiplierGroup nwOps1 (Just (NumberWordOperator (Multiplier "million") op1))):(MultiplierGroup nwOps2 Nothing):[])                                                 -> (+) <$> matchAdderPattern nwOps2 <*> (op1 <*> matchAdderPattern nwOps1)

        ((MultiplierGroup nwOps1 (Just (NumberWordOperator (Multiplier "million") op1))):(MultiplierGroup nwOps2 (Just (NumberWordOperator (Multiplier "thousand") op2))):(MultiplierGroup nwOps3 Nothing):[]) -> (+) <$> matchAdderPattern nwOps3 <*> ((+) <$> (op2 <*> matchAdderPattern nwOps2) <*> (op1 <*> matchAdderPattern nwOps1))

        _ -> Nothing

matchAdderPattern :: [NumberWordOperator] -> Maybe Int
matchAdderPattern nwOps =
    case nwOps of
        ((NumberWordOperator (Zero _) op1):[])        -> op1 <*> pure 0
        ((NumberWordOperator (Unit _) op1):[])        -> op1 <*> pure 0
        ((NumberWordOperator (BelowTwenty _) op1):[]) -> op1 <*> pure 0
        ((NumberWordOperator (AboveTwenty _) op1):[]) -> op1 <*> pure 0

        ((NumberWordOperator (AboveTwenty _) op1):(NumberWordOperator (Unit _) op2):[]) -> op1 <*> (op2 <*> pure 0)
        ((NumberWordOperator (Unit _) op1):(NumberWordOperator (Hundred _) op2):[])     -> op2 <*> (op1 <*> pure 0)

        ((NumberWordOperator (Unit _) op1):(NumberWordOperator (Hundred _) op2):(NumberWordOperator (Unit _) op3):[])        -> op3 <*> (op2 <*> (op1 <*> pure 0))
        ((NumberWordOperator (Unit _) op1):(NumberWordOperator (Hundred _) op2):(NumberWordOperator (BelowTwenty _) op3):[]) -> op3 <*> (op2 <*> (op1 <*> pure 0))
        ((NumberWordOperator (Unit _) op1):(NumberWordOperator (Hundred _) op2):(NumberWordOperator (AboveTwenty _) op3):[]) -> op3 <*> (op2 <*> (op1 <*> pure 0))

        ((NumberWordOperator (Unit _) op1):(NumberWordOperator (Hundred _) op2):(NumberWordOperator (AboveTwenty _) op3):(NumberWordOperator (Unit _) op4):[]) -> op4 <*> (op3 <*> (op2 <*> (op1 <*> pure 0)))

        _ -> Nothing

groupByMultipliers :: [NumberWordOperator] -> [MultiplierGroup]
groupByMultipliers = foldr f []
    where 
        f (NumberWordOperator (Multiplier m) _) acc =
            let newEmptyGroup = MultiplierGroup [] (Just $ getType m)
            in
                case acc of
                    []     -> [newEmptyGroup]
                    groups -> (newEmptyGroup):groups
        f nwOp acc =
            case acc of
                []                                   -> [MultiplierGroup [nwOp] Nothing]
                ((MultiplierGroup [] mult):gs)       -> (MultiplierGroup [nwOp] mult):gs
                ((MultiplierGroup (nw:nws) mult):gs) -> (MultiplierGroup (nwOp:nw:nws) mult):gs

parseWords :: [String] -> [NumberWordOperator]
parseWords ws = map getType ws                

getType :: String -> NumberWordOperator
getType word = 
    case words of
        ((nw,op):_) -> NumberWordOperator nw op
        _           -> NumberWordOperator (Invalid word) Nothing
    where
        types        = [Zero,Unit,BelowTwenty,AboveTwenty
                       ,Hundred,Multiplier]        
        combinations = combination word types
        operators    = getOperator <$> combinations
        combOper     = zip combinations operators
        words        = dropWhile (\(_,oper) -> isNothing oper) combOper

combination :: Applicative f => a -> f (a -> b) -> f b
combination word types = types <*> (pure word)

lookForValue :: Eq a => a -> (b -> c) -> [a] -> [b] -> Maybe c
lookForValue w op ws values = op <$> (lookup w $ zip ws values)