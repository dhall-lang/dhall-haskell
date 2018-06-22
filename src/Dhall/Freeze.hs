{-# LANGUAGE OverloadedStrings #-}

module Dhall.Freeze (freeze) where

import Dhall.Core
import Dhall.Import (load, hashExpression)
import Dhall.Parser (exprAndHeaderFromText, Src)
import Dhall.Pretty (annToAnsiStyle)

import System.Console.ANSI (hSupportsANSI)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Text

import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Control.Exception
import qualified Data.Text.IO
import qualified System.IO

opts :: Pretty.LayoutOptions
opts =
    Pretty.defaultLayoutOptions
        { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }

readInput :: Maybe FilePath -> IO Text
readInput = maybe fromStdin Data.Text.IO.readFile
    where 
        fromStdin = System.IO.hSetEncoding System.IO.stdin System.IO.utf8 >> Data.Text.IO.getContents

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
    let layoutOptions = opts
    let stream = Pretty.layoutSmart layoutOptions doc

    case inplace of
        Just f ->
            System.IO.withFile f System.IO.WriteMode (\h ->
                Pretty.renderIO h (annToAnsiStyle <$> stream))

        Nothing -> do
            supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout
            if supportsANSI 
               then 
                 Pretty.renderIO System.IO.stdout (annToAnsiStyle <$> Pretty.layoutSmart opts doc)
               else
                 Pretty.renderIO System.IO.stdout (Pretty.layoutSmart opts (Pretty.unAnnotate doc)) 

freeze :: Maybe FilePath -> IO ()
freeze inplace = do
    expr <- readInput inplace
    parseExpr srcInfo expr >>= freezeExpr >>= writeExpr inplace
        where
            srcInfo = fromMaybe "(stdin)" inplace
