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

import Data.Monoid
import Data.Text    (Text)
import Dhall.Parser (Header (..))
import Lucid
import Prelude      hiding (FilePath)
import Turtle       (FilePath, fp)

import qualified Data.Text
import qualified Turtle

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
    :: (FilePath, Header) -- ^ (source file name, parsed header)
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
    title = Turtle.format fp $ Turtle.filename filePath


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
        ul_ $ mconcat $ map (li_ . toHtml . Turtle.format fp) files

  where
    title = Turtle.format fp dir <> " index"

stylesheet :: FilePath -> Html ()
stylesheet filePath =
    link_
        [ rel_ "stylesheet"
        , type_ "text/css"
        , href_ $ Turtle.format fp filePath]
