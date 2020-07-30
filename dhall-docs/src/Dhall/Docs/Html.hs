{-| Functions used to generate HTML from a dhall package.
    You can see this module as logic-less HTML building blocks for the whole
    generator tool.

    There are functions that uses `FilePath` instead of `Path a b`. That is because
    the `Path` module doesn't allows to use ".." on its paths and that is needed
    here to properly link css and images.
-}

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Dhall.Docs.Html
    ( dhallFileToHtml
    , indexToHtml
    , DocParams(..)
    ) where

import Data.Text               (Text)
import Data.Void               (Void)
import Dhall.Core              (Expr, Import)
import Dhall.Docs.CodeRenderer
import Dhall.Pretty            (CharacterSet)
import Dhall.Src               (Src)
import Lucid
import Path                    (Dir, File, Path, Rel)

import qualified Control.Monad
import qualified Data.Foldable
import qualified Data.Text
import qualified Path
import qualified System.FilePath as FilePath

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Path (reldir, relfile)

-- | Params for commonly supplied values on the generated documentation
data DocParams = DocParams
    { relativeResourcesPath :: FilePath -- ^ Relative resource path to the
                                        --   front-end files
    , packageName :: Text               -- ^ Name of the package
    , characterSet :: CharacterSet      -- ^ Render code as `ASCII` or `Unicode`
    }

-- | Generates an @`Html` ()@ with all the information about a dhall file
dhallFileToHtml
    :: Path Rel File            -- ^ Source file name, used to extract the title
    -> Text                     -- ^ Contents of the file
    -> Expr Src Import          -- ^ AST of the file
    -> [Expr Void Import]       -- ^ Examples extracted from the assertions of the file
    -> Html ()                  -- ^ Header document as HTML
    -> DocParams                -- ^ Parameters for the documentation
    -> Html ()
dhallFileToHtml filePath contents expr examples header params@DocParams{..} =
    doctypehtml_ $ do
        headContents htmlTitle params
        body_ $ do
            navBar params
            mainContainer $ do
                setPageTitle params NotIndex breadcrumb
                copyToClipboardButton htmlTitle
                br_ []
                div_ [class_ "doc-contents"] header
                Control.Monad.unless (null examples) $ do
                    h3_ "Examples"
                    div_ [class_ "source-code code-examples"] $
                        mapM_ (exprVoidToHtml characterSet AssertionExample) examples
                h3_ "Source"
                div_ [class_ "source-code"] $ exprSrcToHtml contents expr
  where
    breadcrumb = relPathToBreadcrumb filePath
    htmlTitle = breadCrumbsToText breadcrumb

-- | Generates an index @`Html` ()@ that list all the dhall files in that folder
indexToHtml
    :: Path Rel Dir                                -- ^ Index directory
    -> [(Path Rel File, Maybe (Expr Void Import))] -- ^ Generated files in that directory
    -> [Path Rel Dir]                              -- ^ Generated directories in that directory
    -> DocParams                                   -- ^ Parameters for the documentation
    -> Html ()
indexToHtml indexDir files dirs params@DocParams{..} = doctypehtml_ $ do
    headContents htmlTitle params
    body_ $ do
        navBar params
        mainContainer $ do
            setPageTitle params Index breadcrumbs
            copyToClipboardButton htmlTitle
            br_ []
            Control.Monad.unless (null files) $ do
                h3_ "Exported files: "
                ul_ $ mconcat $ map listFile files

            Control.Monad.unless (null dirs) $ do
                h3_ "Exported packages: "
                ul_ $ mconcat $ map listDir dirs

  where
    listFile :: (Path Rel File, Maybe (Expr Void Import)) -> Html ()
    listFile (file, maybeType) =
        let fileRef = Data.Text.pack $ Path.fromRelFile file
            itemText = Data.Text.pack $ tryToTakeExt file
        in li_ $ do
            a_ [href_ fileRef] $ toHtml itemText
            Data.Foldable.forM_ maybeType $ \typeExpr -> do
                span_ [class_ "of-type-token"] ":"
                span_ [class_ "dhall-type source-code"] $ exprVoidToHtml characterSet TypeAnnotation typeExpr


    listDir :: Path Rel Dir -> Html ()
    listDir dir =
        let dirPath = Data.Text.pack $ Path.fromRelDir dir in
        li_ $ a_ [href_ (dirPath <> "index.html")] $ toHtml dirPath

    tryToTakeExt :: Path Rel File -> FilePath

    tryToTakeExt file = Path.fromRelFile $ case Path.splitExtension file of
        Nothing -> file
        Just (f, _) -> f

    breadcrumbs = relPathToBreadcrumb indexDir
    htmlTitle = breadCrumbsToText breadcrumbs

copyToClipboardButton :: Text -> Html ()
copyToClipboardButton filePath =
    a_ [class_ "copy-to-clipboard", data_ "path" filePath]
        $ i_ $ small_ "Copy path to clipboard"


setPageTitle :: DocParams -> HtmlFileType -> Breadcrumb -> Html ()
setPageTitle DocParams{..} htmlFileType breadcrumb =
    h2_ [class_ "doc-title"] $ do
        span_ [class_ "crumb-divider"] "/"
        a_ [href_ $ Data.Text.pack $ relativeResourcesPath <> "index.html"]
            $ toHtml packageName
        breadCrumbsToHtml htmlFileType breadcrumb


-- | ADT for handling bread crumbs. This is essentially a backwards list
--   See `relPathToBreadcrumb` for more information.
data Breadcrumb
    = Crumb Breadcrumb String
    | EmptyCrumb
    deriving Show

data HtmlFileType = NotIndex | Index

{-| Convert a relative path to a `Breadcrumb`.

>>> relPathToBreadcrumb [reldir|a/b/c|]
Crumb (Crumb (Crumb EmptyCrumb "a") "b") "c"
>>> relPathToBreadcrumb [reldir|.|]
Crumb EmptyCrumb ""
>>> relPathToBreadcrumb [relfile|c/foo.baz|]
Crumb (Crumb EmptyCrumb "c") "foo.baz"
-}
relPathToBreadcrumb :: Path Rel a -> Breadcrumb
relPathToBreadcrumb relPath = foldl Crumb EmptyCrumb splittedRelPath
  where
    filePath = Path.toFilePath relPath

    splittedRelPath :: [String]
    splittedRelPath = case FilePath.dropTrailingPathSeparator filePath of
        "." -> [""]
        _ -> FilePath.splitDirectories filePath

-- | Render breadcrumbs as `Html ()`
breadCrumbsToHtml :: HtmlFileType -> Breadcrumb -> Html ()
breadCrumbsToHtml htmlFileType = go startLevel
  where
    startLevel = case htmlFileType of
        NotIndex -> -1
        Index -> 0

    -- copyBreadcrumbButton :: Html ()
    -- copyBreadcrumbButton =
    --     button_
    --         [ class_ "btn copy-breadcrumb"
    --         , data_ "breadcrumb" $ breadCrumbsToText breadcrumb
    --         ] ""

    go :: Int -> Breadcrumb -> Html ()
    go _ EmptyCrumb = return ()
    go level (Crumb bc name) = do
        go (level + 1) bc
        span_ [class_ "crumb-divider"] $ toHtml ("/" :: Text)
        elem_ [class_ "title-crumb", href_ hrefTarget] $ toHtml name
      where
        hrefTarget = Data.Text.replicate level "../" <> "index.html"
        elem_ = if level == startLevel then span_ else a_


-- | Render breadcrumbs as plain text
breadCrumbsToText :: Breadcrumb -> Text
breadCrumbsToText EmptyCrumb = ""
breadCrumbsToText (Crumb bc c) = breadCrumbsToText bc <> "/" <> Data.Text.pack c


-- | nav-bar component of the HTML documentation
navBar
    :: DocParams -- ^ Parameters for doc generation
    -> Html ()
navBar DocParams{..} = div_ [class_ "nav-bar"] $ do

    -- Left side of the nav-bar
    img_ [ class_ "dhall-icon"
         , src_ $ Data.Text.pack $ relativeResourcesPath <> "dhall-icon.svg"
         ]
    p_ [class_ "package-title"] $ toHtml packageName

    div_ [class_ "nav-bar-content-divider"] ""

    -- Right side of the nav-bar
    -- with makeOption [id_ "go-to-source-code"] "Source code"
    with makeOption [id_ "switch-light-dark-mode"] "Switch Light/Dark Mode"
  where
    makeOption = with a_ [class_ "nav-option"]


headContents :: Text -> DocParams -> Html ()
headContents title DocParams{..} =
    head_ $ do
        title_ $ toHtml title
        stylesheet $ relativeResourcesPath <> "index.css"
        stylesheet "https://fonts.googleapis.com/css2?family=Fira+Code:wght@400;500;600;700&family=Lato:ital,wght@0,400;0,700;1,400&display=swap"
        script relativeResourcesPath
        meta_ [charset_ "UTF-8"]

-- | main-container component builder of the HTML documentation
mainContainer :: Html() -> Html ()
mainContainer = div_ [class_ "main-container"]

stylesheet :: FilePath -> Html ()
stylesheet path =
    link_
        [ rel_ "stylesheet"
        , type_ "text/css"
        , href_ $ Data.Text.pack path]

script :: FilePath -> Html ()
script relativeResourcesPath =
    script_
        [ type_ "text/javascript"
        , src_ $ Data.Text.pack $ relativeResourcesPath <> "index.js"]
        ("" :: Text)
