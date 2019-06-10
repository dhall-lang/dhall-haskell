{-| This module contains the top-level entrypoint and options parsing for the
    @dhall-lsp-server@ executable
-}

module Main
  ( main
  )
where

import Options.Applicative (Parser, ParserInfo)
import qualified Options.Applicative
import Control.Applicative ((<|>))

import System.Log (Priority(..))
import qualified Dhall.LSP.Server

-- | Top-level program options
data Options = Options {
    command :: Mode
  , logging :: Logging
}

data Logging = Logging {
    logFile :: Maybe FilePath
  , logLevel :: Priority  
}

-- | The mode in which to run @dhall-lsp-server@
data Mode = Version | LSPServer

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseMode
    <*> (Logging <$> Options.Applicative.optional parseLogFile <*> parseLogLevel
        )
  where
    parseLogFile = Options.Applicative.strOption
      (Options.Applicative.long "log" <> Options.Applicative.help
        "Redirect log out put to the specified file (defaults to stderr)"
      )


subcommand :: String -> String -> Parser a -> Parser a
subcommand name description parser = Options.Applicative.hsubparser
  (Options.Applicative.command name parserInfo
  <> Options.Applicative.metavar name
  )
  where
    parserInfo = Options.Applicative.info
      parser
      (Options.Applicative.fullDesc <> Options.Applicative.progDesc description)

parseLogLevel :: Parser Priority
parseLogLevel =
  Options.Applicative.option
      Options.Applicative.auto
      (  Options.Applicative.long "loglevel"
      <> Options.Applicative.help "Set log level (defaults to ERROR). Possible\
           \ values are: DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL,\
           \ ALTERT, EMERGENCY (see System.Log)."
      )
    <|> pure ERROR

parseMode :: Parser Mode
parseMode =
  subcommand "version" "Display version" (pure Version) <|> pure LSPServer

parserInfoOptions :: ParserInfo Options
parserInfoOptions = Options.Applicative.info
  (Options.Applicative.helper <*> parseOptions)
  (Options.Applicative.progDesc "LSP server for the Dhall language"
  <> Options.Applicative.fullDesc
  )

runCommand :: Options -> IO ()
runCommand Options {..} = case command of
  Version -> putStrLn ("0.0.1.1" :: String)
  LSPServer -> let Logging{..} = logging
               in Dhall.LSP.Server.run logFile logLevel

-- | Entry point for the @dhall-lsp-server@ executable
main :: IO ()
main = do options <- Options.Applicative.execParser parserInfoOptions
          runCommand options
