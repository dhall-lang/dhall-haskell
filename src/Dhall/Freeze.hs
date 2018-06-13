{-# LANGUAGE OverloadedStrings #-}

module Dhall.Freeze (freeze) where

import Dhall.Core
import Dhall.Import (load, hashExpression)
import Dhall.Parser (exprAndHeaderFromText)
import Data.Text.Prettyprint.Doc (Pretty)
import Dhall.Pretty (prettyExpr)

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

render :: Pretty a => Expr s a -> Text
render e = do
    let doc = prettyExpr e
    let layoutOptions = opts
    let stream = Pretty.layoutSmart layoutOptions doc
    let ansiStream = Pretty.unAnnotateS stream
    Pretty.renderStrict ansiStream


readInput :: Maybe FilePath -> IO Text
readInput = maybe fromStdin Data.Text.IO.readFile
    where 
        fromStdin  = System.IO.hSetEncoding System.IO.stdin System.IO.utf8 >> Data.Text.IO.getContents

writeFrozen :: Maybe FilePath -> Text -> IO ()
writeFrozen inplace txt = maybe (toStdout txt) (writeToFile txt) inplace
    where
        toStdout = Data.Text.IO.putStrLn
        writeToFile text path = Data.Text.IO.writeFile path text

hashImport :: Import -> IO Import
hashImport import_ = do
    expression <- Dhall.Import.load (Embed import_)
    let expressionHash = Just (Dhall.Import.hashExpression expression)
    let newImportHashed = (importHashed import_) { hash = expressionHash }
    return $ import_ { importHashed = newImportHashed }

freezeExpression :: Text -> IO Text
freezeExpression txt = do
    (_, expr) <- case exprAndHeaderFromText "" txt of
        Left err -> Control.Exception.throwIO err
        Right x  -> return x
    frozenExpr <- traverse hashImport expr
    return $ render frozenExpr


freeze :: Maybe FilePath -> IO ()
freeze inplace = readExpression >>= freezeExpression >>= writeExpression
    where
        readExpression = readInput inplace
        writeExpression = writeFrozen inplace
