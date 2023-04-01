{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Pretty
    ( pathCharacter
    ) where

import                Data.Text             (Text)
import {-# SOURCE #-} Dhall.Pretty.Internal
import                Dhall.Syntax.Const
import                Dhall.Syntax.Expr
import                Dhall.Syntax.Import
import                Dhall.Syntax.Var
import                Prettyprinter         (Doc, Pretty)

import qualified Data.Text
import qualified Network.URI   as URI
import qualified Prettyprinter as Pretty

instance Pretty Const where
    pretty = Pretty.unAnnotate . prettyConst

instance Pretty Var where
    pretty = Pretty.unAnnotate . prettyVar

-- | Generates a syntactically valid Dhall program
instance Pretty a => Pretty (Expr s a) where
    pretty = Pretty.unAnnotate . prettyExpr

instance Pretty Directory where
    pretty (Directory {..}) = foldMap prettyPathComponent (reverse components)

prettyPathComponent :: Text -> Doc ann
prettyPathComponent text
    | Data.Text.all pathCharacter text =
        "/" <> Pretty.pretty text
    | otherwise =
        "/\"" <> Pretty.pretty text <> "\""

instance Pretty File where
    pretty (File {..}) =
            Pretty.pretty directory
        <>  prettyPathComponent file

instance Pretty FilePrefix where
    pretty Absolute = ""
    pretty Here     = "."
    pretty Parent   = ".."
    pretty Home     = "~"

instance Pretty URL where
    pretty (URL {..}) =
            schemeDoc
        <>  "://"
        <>  Pretty.pretty authority
        <>  pathDoc
        <>  queryDoc
        <>  foldMap prettyHeaders headers
      where
        prettyHeaders h =
          " using (" <> Pretty.unAnnotate (Pretty.pretty h) <> ")"

        File {..} = path

        Directory {..} = directory

        pathDoc =
                foldMap prettyURIComponent (reverse components)
            <>  prettyURIComponent file

        schemeDoc = case scheme of
            HTTP  -> "http"
            HTTPS -> "https"

        queryDoc = case query of
            Nothing -> ""
            Just q  -> "?" <> Pretty.pretty q

prettyURIComponent :: Text -> Doc ann
prettyURIComponent text =
        Pretty.pretty $ URI.normalizeCase $ URI.normalizeEscape $ "/" <> Data.Text.unpack text

instance Pretty ImportType where
    pretty (Local prefix file) =
        Pretty.pretty prefix <> Pretty.pretty file

    pretty (Remote url) = Pretty.pretty url

    pretty (Env env) = "env:" <> prettyEnvironmentVariable env

    pretty Missing = "missing"

instance Pretty ImportHashed where
    pretty (ImportHashed  Nothing p) =
      Pretty.pretty p
    pretty (ImportHashed (Just h) p) =
      Pretty.group (Pretty.flatAlt long short)
      where
        long =
            Pretty.align
                (   Pretty.pretty p <> Pretty.hardline
                <>  "  sha256:" <> Pretty.pretty (show h)
                )

        short = Pretty.pretty p <> " sha256:" <> Pretty.pretty (show h)

instance Pretty Import where
    pretty (Import {..}) = Pretty.pretty importHashed <> Pretty.pretty suffix
      where
        suffix :: Text
        suffix = case importMode of
            RawText  -> " as Text"
            Location -> " as Location"
            Code     -> ""
            RawBytes -> " as Bytes"

{-| Returns `True` if the given `Char` is valid within an unquoted path
    component

    This is exported for reuse within the @"Dhall.Parser.Token"@ module
-}
pathCharacter :: Char -> Bool
pathCharacter c =
         '\x21' == c
    ||  ('\x24' <= c && c <= '\x27')
    ||  ('\x2A' <= c && c <= '\x2B')
    ||  ('\x2D' <= c && c <= '\x2E')
    ||  ('\x30' <= c && c <= '\x3B')
    ||  c == '\x3D'
    ||  ('\x40' <= c && c <= '\x5A')
    ||  ('\x5E' <= c && c <= '\x7A')
    ||  c == '\x7C'
    ||  c == '\x7E'
