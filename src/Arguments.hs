module Arguments (Arguments (Args), readArguments) where

import Data.Semigroup ((<>))
import Options.Applicative
import System.FilePath (FilePath)

data Arguments = Args {
      inputFile :: FilePath
    , outputFile :: FilePath
    , quiet :: Bool }

appInfo :: Parser a -> ParserInfo a
appInfo = flip info $
       fullDesc
    <> header "numeric-dictionary"
    <> progDesc "Transforms numbers written in English to their numeric representation"

arguments :: Parser Arguments
arguments = Args <$>
    argument auto (
           metavar "INPUT_FILE"
        <> help "Path to the file that contains the numbers written in English")
    <*> argument auto (
           metavar "OUTPUT_FILE"
        <> help "Path to the file the transformations will be written to")
    <*> switch ( 
           long "quiet"
        <> short 'q'
        <> help "Whether to print the transformations to the standard output")

argsWithAppInfo :: ParserInfo Arguments
argsWithAppInfo = appInfo $ arguments <**> helper

readArguments :: IO Arguments
readArguments = execParser argsWithAppInfo
