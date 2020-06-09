{-| Functions used to generate HTML from a dhall package.
    You can see this module as logic-less HTML building blocks for the whole
    generator tool
-}

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Dhall.Docs.Html
    ( headerToHtml
    , filePathHeaderToHtml
    , indexToHtml
    ) where

import Data.Text    (Text)
import Dhall.Parser (Header (..))
import Lucid
import Path         (Abs, File, Path)

import qualified Data.Text
import qualified Path

-- | Takes a `Header` and generates its `Html`
headerToHtml :: Header -> Html ()
headerToHtml = p_ . toHtml . removeComments

removeComments :: Header -> Text
removeComments (Header header)
    | "--" `Data.Text.isPrefixOf` strippedHeader = Data.Text.drop 2 strippedHeader
    | "{-" `Data.Text.isPrefixOf` strippedHeader =
        Data.Text.drop 2 $ Data.Text.dropEnd 2 strippedHeader
    | otherwise = strippedHeader

  where
    strippedHeader = Data.Text.strip header

-- | Generates an @`Html` ()@ with all the information about a dhall file
filePathHeaderToHtml
    :: (Path Abs File, Header) -- ^ (source file name, parsed header)
    -> FilePath           -- ^ Relative path to the css of the tool
    -> Html ()
filePathHeaderToHtml (filePath, header) cssFile =
    html_ $ do
        head_ $ do
            title_ $ toHtml title
            stylesheet cssFile
        body_ $ do
            h1_ $ toHtml title
            headerToHtml header
  where
    title = Path.fromRelFile $ Path.filename filePath


-- | Generates an index @`Html` ()@ that list all the dhall files in that folder
indexToHtml
    :: FilePath   -- ^ Index directory
    -> [FilePath] -- ^ Generated files in that directory
    -> FilePath   -- ^ Relative path to the css of the tool
    -> Html ()
indexToHtml dir files cssFile = html_ $ do
    head_ $ do
        title_ $ toHtml title
        stylesheet cssFile
    body_ $ do
        h1_ $ toHtml title
        p_ "Exported files: "
        ul_ $ mconcat $ map (li_ . toHtml) files

  where
    title = dir <> " index"

stylesheet :: FilePath -> Html ()
stylesheet filePath =
    link_
        [ rel_ "stylesheet"
        , type_ "text/css"
        , href_ $ Data.Text.pack filePath]
