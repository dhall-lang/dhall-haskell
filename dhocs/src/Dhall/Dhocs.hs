{-| This module contains the top level and options parsing of the @dhocs@ executable
-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.Dhocs
    ( -- * Options
      Options(..)
    , GenerationStrategy(..)
    , parserInfoOptions
    , parseOptions

      -- * Execution
    , main
    ) where

import Data.Semigroup      ((<>))
import Options.Applicative


-- | To specify if we should generate a single HTML page with all your package info
--   or one for each file in your package
data GenerationStrategy
    = SinglePage
    | MultiPage
    deriving Show

-- | Command line options
data Options = Options
    { packageFile :: FilePath -- ^ `FilePath` to directory where your package resides
    , strategy :: GenerationStrategy -- ^ Output strategy of the tool
    }
    deriving Show

parseStrategy :: Parser GenerationStrategy
parseStrategy =
    Options.Applicative.flag
        MultiPage
        SinglePage
        (   Options.Applicative.long "single-page"
        <>  Options.Applicative.help
                "Generate a single page HTML documentation. By default, it will generate \
                \a multi-page documentation"
        )

-- | `Parser` for the `Options` type
parseOptions :: Parser Options
parseOptions =
        Options
    <$> strOption
        ( long "path"
       <> short 'p'
       <> metavar "PATH"
       <> help "Root folder of your dhall package" )
    <*> parseStrategy

-- | `ParserInfo` for the `Options` type
parserInfoOptions :: ParserInfo Options
parserInfoOptions =
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.fullDesc
        <>  Options.Applicative.progDesc "Generate HTML documentation from a dhall package or file"
        )

-- |
main :: IO ()
main = Options.Applicative.execParser parserInfoOptions >>= print
