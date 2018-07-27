{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the implementation of the @dhall freeze@ subcommand

module Dhall.Freeze
    ( -- * Freeze
      freeze
    , hashImport
    ) where

import Dhall.Core
import Dhall.Import (load, hashExpression)
import Dhall.Parser (exprAndHeaderFromText, Src)
import Dhall.Pretty (annToAnsiStyle, layoutOpts)

import System.Console.ANSI (hSupportsANSI)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Text

import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Control.Exception
import qualified Data.Text.IO
import qualified System.IO

readInput :: Maybe FilePath -> IO Text
readInput = maybe Data.Text.IO.getContents Data.Text.IO.readFile

-- | Retrieve an `Import` and update the hash to match the latest contents
hashImport :: Import -> IO Import
hashImport import_ = do
    expression <- Dhall.Import.load (Embed import_)
    let expressionHash = Just (Dhall.Import.hashExpression expression)
    let newImportHashed = (importHashed import_) { hash = expressionHash }
    return $ import_ { importHashed = newImportHashed }

parseExpr :: String -> Text -> IO (Text, Expr Src Import)
parseExpr src txt =
    case exprAndHeaderFromText src txt of
        Left err -> Control.Exception.throwIO err
        Right x  -> return x

freezeExpr :: (Text, Expr s Import) -> IO (Text, Expr s Import)
freezeExpr (t, e) = do
    e' <- traverse hashImport e
    return (t, e')

writeExpr :: Maybe FilePath -> (Text, Expr s Import) -> IO ()
writeExpr inplace (header, expr) = do
    let doc = Pretty.pretty header <> Pretty.pretty expr
    let stream = Pretty.layoutSmart layoutOpts doc

    case inplace of
        Just f ->
            System.IO.withFile f System.IO.WriteMode (\h ->
                Pretty.renderIO h (annToAnsiStyle <$> stream))

        Nothing -> do
            supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout
            if supportsANSI 
               then 
                 Pretty.renderIO System.IO.stdout (annToAnsiStyle <$> Pretty.layoutSmart layoutOpts doc)
               else
                 Pretty.renderIO System.IO.stdout (Pretty.layoutSmart layoutOpts (Pretty.unAnnotate doc)) 

-- | Implementation of the @dhall freeze@ subcommand
freeze
    :: Maybe FilePath
    -- ^ Modify file in-place if present, otherwise read from @stdin@ and write
    --   to @stdout@
    -> IO ()
freeze inplace = do
    expr <- readInput inplace
    parseExpr srcInfo expr >>= freezeExpr >>= writeExpr inplace
        where
            srcInfo = fromMaybe "(stdin)" inplace
