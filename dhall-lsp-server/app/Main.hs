
module Main (main) where




import           Data.Default
import qualified Data.HashMap.Strict                   as H
import qualified Data.Text                             as T
import qualified System.Exit
import qualified System.Log.Logger                     as L



import qualified Data.Text.IO
import qualified System.IO
import qualified Data.Map as Map
import Options.Applicative (Parser, ParserInfo)
import qualified Options.Applicative

import qualified System.IO.Unsafe

import LSP.Server(run)

data Options = Options {
    command :: Command
  , logFile :: Maybe String -- file where the server process debug log should be written
}

data Command = CmdVersion | Default

parseOptions :: Parser Options
parseOptions = Options <$> parseMode
                       <*> Options.Applicative.optional parseLogFile
    where
      parseLogFile = Options.Applicative.strOption
                       (
                          Options.Applicative.long "log"
                       <> Options.Applicative.help "If present writes debug output to the specified file")


subcommand :: String -> String -> Parser a -> Parser a
subcommand name description parser =
    Options.Applicative.hsubparser
        (   Options.Applicative.command name parserInfo
        <>  Options.Applicative.metavar name
        )
  where
    parserInfo =
        Options.Applicative.info parser
            (   Options.Applicative.fullDesc
            <>  Options.Applicative.progDesc description
            )

parseMode :: Parser Command
parseMode =
        subcommand
            "version"
            "Display version"
            (pure CmdVersion)
    <|> pure Default

parserInfoOptions :: ParserInfo Options
parserInfoOptions =
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.progDesc "Interpreter for the Dhall language"
        <>  Options.Applicative.fullDesc
        )

runCommand :: Options -> IO ()
runCommand Options{..} = case command of
  CmdVersion -> putStrLn ("0.0.1.1" :: String)-- TODO: read from build
  Default    ->
   run logFile (pure ()) >>= \case
    0 -> exitSuccess
    c -> exitWith . System.Exit.ExitFailure $ c

main :: IO ()
main = Options.Applicative.execParser parserInfoOptions >>= runCommand


