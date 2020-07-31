{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-| Contains the logic to render the source code inside a HTML. It also provides
    context-sensitive features such as jump-to-definition
-}
module Dhall.Docs.CodeRenderer
    ( exprSrcToHtml
    , exprVoidToHtml
    , ExprType(..)
    ) where

import Data.Text       (Text)
import Data.Void       (Void)
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
import Dhall.Docs.Util
import Dhall.Src       (Src (..))
import Lucid

import qualified Data.Text
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty.Text
import qualified Dhall.Core                            as Core
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Lens.Family                           as Lens
import qualified Text.Megaparsec.Pos                   as SourcePos

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Dhall.Core (Directory (..))

-- | Given a Dhall expression return all imports with their location on the file.
--   Contents are already sorted by 'Src', which allows 'renderAsHtml' to traverse
--   the return of this function to inject anchors
getImports :: Expr Src Import -> [(Src, Import)]
getImports (Note src (Embed a)) = [(src, a)]
getImports expr = concatMap getImports $ Lens.toListOf Core.subExpressions expr

fileAsText :: File -> Text
fileAsText File{..} = foldr (\d acc -> acc <> "/" <> d) "" (Core.components directory)
    <> "/" <> file

{-| Given an 'Import', render the contents in an HTML element that will allow
    users to jump to another file or domain. The 'Text' argument is the contents
    inside the anchor tag

    Example:

    >>> :set -Wno-missing-fields
    >>> let file = File { directory = Directory [], file = ""}
    >>> let url = URL { scheme = HTTPS, authority = "google.com", query = Nothing, path = file}
    >>> let import_ = Import {importHashed = ImportHashed { importType = Remote url }}
    >>> renderImport import_ "link for google"
    <a href="https://google.com/" target="_blank">link for google</a>
-}
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
            href = "." <> fileAsText file <> ".html"

        Local Parent file -> a_ [href_ href] . toHtml
          where
            href = ".." <> fileAsText file <> ".html"

        _ -> toHtml

-- | Given a Text and the parsed `Expr Src Import` from it, this will render the
--   the source code on HTML with jump-to-definition on URL imports
exprSrcToHtml :: Text -> Expr Src Import -> Html ()
exprSrcToHtml contents expr = pre_ $ go (1, 1) (Data.Text.lines contents) imports
  where
    imports = getImports expr

    -- we keep the current line, column and consumed text as part of function argument
    go :: (Int, Int) -> [Text] -> [(Src, Import)] -> Html ()
    go _ textLines [] = mapM_ (\t -> toHtml t >> br_ []) textLines
    go (currLine, currCol) currentLines ((Src {..}, import_) : rest) = do
        let sourceLine = SourcePos.unPos . SourcePos.sourceLine
        let sourceColumn = SourcePos.unPos . SourcePos.sourceColumn

        let importStartLine = sourceLine srcStart
        let importEndLine = sourceLine srcEnd

        let importStartCol = sourceColumn srcStart
        let importEndCol = sourceColumn srcEnd

        let (prefixLines, restLines) = splitAt (importStartLine - currLine) currentLines
        let (importLines, suffixLines) = splitAt (importStartLine - importStartLine + 1) restLines

        -- calls to `head` and `last` here should never fail since `importLines`
        -- have at least one element
        let (firstImportLine, lastImportLine) = (head importLines, last importLines)
        let prefixCols = Data.Text.take (importStartCol - currCol) firstImportLine
        let suffixCols = Data.Text.drop (importEndCol - currCol) lastImportLine

        -- prefix lines and columns
        mapM_ (\t -> toHtml t >> br_ []) prefixLines
        toHtml prefixCols

        -- rendered import
        renderImport import_ srcText

        -- add a newline if last line of import consumes the remaining line on
        -- the original text
        if Data.Text.null suffixCols then br_ [] else return ()

        let suffix = if Data.Text.null suffixCols then suffixLines else suffixCols : suffixLines

        -- move the cursor to next line if no characterse are remaining on the
        -- suffix cols, otherwise keep the last line and next char right after
        -- the import. This is done to handle properly several imports on the
        -- same line
        let nextPosition = if Data.Text.null suffixCols then
                               (importEndLine + 1, 1)
                           else (importEndLine, importEndCol)

        go nextPosition suffix rest

-- | Internal utility to differentiate if a Dhall expr is a type annotation
--   or the whole file
data ExprType = TypeAnnotation | AssertionExample

-- | Renders an AST /fragment/ from the source file AST. Use this only for that
--   purpose. The difference between this and 'exprSrcToHtml' is because that
--   the extracted fragment's 'SourcePos's need to be re-generated to
--   render them in a better way; just adding whitespace at the beginning of the
--   first line won't render good results.
exprVoidToHtml :: Dhall.Pretty.CharacterSet -> ExprType -> Expr Void Import -> Html ()
exprVoidToHtml characterSet exprType expr = exprSrcToHtml formattedFile expr'
  where
    layout = case exprType of
        AssertionExample -> Dhall.Pretty.layout
        TypeAnnotation -> typeLayout

    formattedFile = Pretty.Text.renderStrict
        $ layout
        $ Dhall.Pretty.prettyCharacterSet characterSet (Core.denote expr)

    expr' = case Dhall.Parser.exprFromText "" formattedFile of
        Right e -> e
        Left _ -> fileAnIssue "A failure has occurred while parsing a formatted file"

    typeLayout :: Pretty.Doc ann -> Pretty.SimpleDocStream ann
    typeLayout = Pretty.removeTrailingWhitespace . Pretty.layoutSmart opts
      where
        -- this is done so the type of a dhall file fits in a single line
        -- its a safe value, since types in source codes are not that large
        opts :: Pretty.LayoutOptions
        opts = Pretty.defaultLayoutOptions
                { Pretty.layoutPageWidth =
                    Pretty.Unbounded
                }
