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
    ( filePathHeaderToHtml
    , indexToHtml
    , DocParams(..)
    ) where

import Data.Monoid ((<>))
import Data.Text   (Text)
import Lucid
import Path        (Abs, Dir, File, Path, Rel)

import qualified Control.Monad
import qualified Data.Text
import qualified Path

-- | Params for commonly supplied values on the generated documentation
data DocParams = DocParams
    { relativeResourcesPath :: !FilePath -- ^ Relative resource path to the
                                        --   front-end files
    , packageName :: !String               -- ^ Name of the package
    }

-- | Generates an @`Html` ()@ with all the information about a dhall file
filePathHeaderToHtml
    :: (Path Abs File, Html ()) -- ^ (source file name, parsed header)
    -> DocParams                -- ^ Parameters for the documentation
    -> Html ()
filePathHeaderToHtml (filePath, header) params@DocParams{..} =
    doctypehtml_ $ do
        headContents title params
        body_ $ do
            navBar params
            mainContainer $ do
                h1_ $ toHtml title
                div_ [class_ "doc-contents"] header
  where
    title = Path.fromRelFile $ Path.filename filePath


-- | Generates an index @`Html` ()@ that list all the dhall files in that folder
indexToHtml
    :: FilePath        -- ^ Index directory
    -> [Path Rel File] -- ^ Generated files in that directory
    -> [Path Rel Dir]  -- ^ Generated directories in that directory
    -> DocParams       -- ^ Parameters for the documentation
    -> Html ()
indexToHtml indexDir files dirs params@DocParams{..} = doctypehtml_ $ do
    headContents title params
    body_ $ do
        navBar params
        mainContainer $ do
            h1_ $ toHtml title

            Control.Monad.unless (null files) $ do
                h2_ "Exported files: "
                ul_ $ mconcat $ map listFile files

            Control.Monad.unless (null dirs) $ do
                h2_ "Exported packages: "
                ul_ $ mconcat $ map listDir dirs

  where
    listFile :: Path Rel File -> Html ()
    listFile file =
        let fileRef = Data.Text.pack $ Path.fromRelFile file
            itemText = Data.Text.pack $ tryToTakeExt file
        in li_ $ a_ [href_ fileRef] $ toHtml itemText

    listDir :: Path Rel Dir -> Html ()
    listDir dir =
        let filePath = Data.Text.pack $ Path.fromRelDir dir in
        li_ $ a_ [href_ (filePath <> "index.html")] $ toHtml filePath

    tryToTakeExt :: Path Rel File -> FilePath

    tryToTakeExt file = Path.fromRelFile $ case Path.splitExtension file of
        Nothing -> file
        Just (f, _) -> f

    title :: String
    title = indexDir <> " index"

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
    with makeOption
        [ id_ "go-to-index"
        , href_ $ Data.Text.pack $ relativeResourcesPath <> "index.html"
        ] "Go to package index"
  where
    makeOption = with a_ [class_ "nav-option"]


headContents :: String -> DocParams -> Html ()
headContents title DocParams{..} =
    head_ $ do
        title_ $ toHtml title
        stylesheet relativeResourcesPath
        script relativeResourcesPath

-- | main-container component builder of the HTML documentation
mainContainer :: Html() -> Html ()
mainContainer = div_ [class_ "main-container"]

stylesheet :: FilePath -> Html ()
stylesheet relativeResourcesPath =
    link_
        [ rel_ "stylesheet"
        , type_ "text/css"
        , href_ $ Data.Text.pack $ relativeResourcesPath <> "index.css"]

script :: FilePath -> Html ()
script relativeResourcesPath =
    script_
        [ type_ "text/javascript"
        , src_ $ Data.Text.pack $ relativeResourcesPath <> "index.js"]
        ("empty" :: Text)
