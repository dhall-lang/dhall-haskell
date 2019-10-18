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
import Data.Monoid ((<>))

import qualified Data.Version
import qualified Dhall.LSP.Server
import qualified Paths_dhall_lsp_server
import qualified GHC.IO.Encoding

-- | Top-level program options
data Options = Options {
    command :: Mode
  , logFile :: Maybe FilePath
  , version :: Bool
}

-- | The mode in which to run @dhall-lsp-server@
data Mode = Version | LSPServer

parseOptions :: Parser Options
parseOptions =
  Options <$> parseMode <*> Options.Applicative.optional parseLogFile <*> parseVersion
  where
    parseLogFile = Options.Applicative.strOption
      (Options.Applicative.long "log" <> Options.Applicative.help
        "If present writes debug output to the specified file"
      )

    parseVersion = Options.Applicative.switch
      (  Options.Applicative.long "version"
      <> Options.Applicative.help "Display version"
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
runCommand Options {..} = do
  if version
    then printVersion
    else case command of
      Version -> printVersion
      LSPServer -> Dhall.LSP.Server.run logFile
  where
    printVersion = putStrLn (Data.Version.showVersion Paths_dhall_lsp_server.version)

-- | Entry point for the @dhall-lsp-server@ executable
main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    options <- Options.Applicative.execParser parserInfoOptions

    runCommand options
