{-# LANGUAGE RecordWildCards #-}

import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Options.Applicative (Parser, ParserInfo)

import qualified Data.Text.IO
import qualified Data.Version
import qualified Dhall
import qualified Options.Applicative
import qualified Paths_dhall_text

data Options
    = Default { explain :: Bool }
    | Version

parseOptions :: Parser Options
parseOptions = parseVersion <|> parseDefault
  where
    parseVersion =
        Options.Applicative.subparser
            (   Options.Applicative.command "version" parserInfoVersion
            <>  Options.Applicative.metavar "version"
            )
      where
        parserInfoVersion =
            Options.Applicative.info parser
                (   Options.Applicative.fullDesc
                <>  Options.Applicative.progDesc "Display version"
                )

        parser =
            Options.Applicative.helper <*> pure Version

    parseDefault = Default <$> parseExplain
      where
        parseExplain =
            Options.Applicative.switch
                (   Options.Applicative.long "explain"
                <>  Options.Applicative.help "Explain error messages in detail"
                )

parserInfoOptions :: ParserInfo Options
parserInfoOptions =
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.progDesc "Template text using Dhall"
        <>  Options.Applicative.fullDesc
        )

main :: IO ()
main = do
    options <- Options.Applicative.execParser parserInfoOptions

    case options of
        Default {..} -> do
            let detail = if explain then Dhall.detailed else id
            code <- Data.Text.IO.getContents
            text <- detail (Dhall.input Dhall.auto code)
            Data.Text.IO.putStr text

        Version -> do
            putStrLn (Data.Version.showVersion Paths_dhall_text.version)
