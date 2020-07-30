{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-| Contains the logic to render the source code inside a HTML. It also provides
    context-sensitive features such as jump-to-definition
-}
module Dhall.Docs.CodeRenderer (renderExpr) where

import Data.Text  (Text)
import Dhall.Core
    ( Expr (..)
    , File (..)
    , FilePrefix (..)
    , Import (..)
    , ImportHashed (..)
    , ImportType (..)
    , Scheme (..)
    , URL (..)
    )
import Dhall.Src  (Src (..))
import Lucid

import qualified Data.Text
import qualified Dhall.Core          as Core
import qualified Lens.Family         as Lens
import qualified Text.Megaparsec.Pos as SourcePos

-- | Given a Dhall expression return all imports with their location on the file.
--   Contents are already sorted by 'Src', which allows 'renderAsHtml' to traverse
--   the return of this function to inject anchors
getImports :: Expr Src Import -> [(Src, Import)]
getImports (Note src (Embed a)) = [(src, a)]
getImports expr = concatMap getImports $ Lens.toListOf Core.subExpressions expr

fileAsText :: File -> Text
fileAsText File{..} = foldr (\d acc -> acc <> "/" <> d) "" (Core.components directory)
    <> "/" <> file

-- | Given an 'Import', render the contents in an HTML element that will allow
--   users to jump to another file or domain
renderImport :: Import -> Text -> Html ()
renderImport (Import {importHashed = ImportHashed { importType }}) =
    case importType of
        Remote URL {..} -> a_ [href_ href, target_ "_blank"] . toHtml
          where
            scheme_ = case scheme of
                HTTP -> "http"
                HTTPS -> "https"

            path_ = fileAsText path

            query_ = case query of
                Nothing -> ""
                Just d -> "?" <> d

            -- we don't include the headers here since we treat links to open a file
            -- in another tab
            href = scheme_ <> "://" <> authority <> path_ <> query_

        Local Here file -> a_ [href_ href] . toHtml
          where
            href = "./" <> fileAsText file <> ".html"

        _ -> toHtml

-- | Given a Text and the parsed `Expr Src Import` from it, this will render the
--   the source code on HTML with jump-to-definition on URL imports
renderExpr :: Text -> Expr Src Import -> Html ()
renderExpr contents expr = pre_ $ go (1, 1) (Data.Text.lines contents) imports
  where
    imports = getImports expr
    sourceLine = SourcePos.unPos . SourcePos.sourceLine
    sourceColumn = SourcePos.unPos . SourcePos.sourceColumn

    -- we keep the current line, column and consumed text as part of function argument
    go :: (Int, Int) -> [Text] -> [(Src, Import)] -> Html ()
    go _ textLines [] = mapM_ (\t -> toHtml t >> br_ []) textLines
    go (currLine, currCol) currentLines ((Src {..}, import_) : rest) = do
        prefix
        renderImport import_ srcText
        if Data.Text.null suffixCols then br_ [] else return ()
        go nextPosition suffix rest
      where
        importStartLine = sourceLine srcStart
        importEndLine = sourceLine srcEnd

        importStartCol = sourceColumn srcStart
        importEndCol = sourceColumn srcEnd

        (prefixLines, restLines) = splitAt (importStartLine - currLine) currentLines
        (importLines, suffixLines) = splitAt (importStartLine - importStartLine + 1) restLines

        -- calls to `head` and `last` here should never fail since `importLines`
        -- have at least one element
        (firstImportLine, lastImportLine) = (head importLines, last importLines)
        prefixCols = Data.Text.take (importStartCol - currCol) firstImportLine
        suffixCols = Data.Text.drop (importEndCol - currCol) lastImportLine

        prefix = mapM_ (\t -> toHtml t >> br_ []) prefixLines >> toHtml prefixCols
        suffix = if Data.Text.null suffixCols then suffixLines else suffixCols : suffixLines
        nextPosition =
            if Data.Text.null suffixCols then
                (importEndLine + 1, 1)
            else (importEndLine, importEndCol)
