{-| This module expors all functions used to generate HTML from a dhall package
-}

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Dhall.Docs.Html (headerToHtml, filePathHeaderToHtml) where

import Data.Text
import Dhall.Parser (Header (..))
import Lucid
import Prelude      hiding (FilePath)
import Turtle       (FilePath, fp)

import qualified Turtle

-- | Takes a `Header` and generates its `Html`
headerToHtml :: Header -> Html ()
headerToHtml = p_ . toHtml . removeComments

removeComments :: Header -> Text
removeComments (Header header)
    | "--" `isPrefixOf` strippedHeader = Data.Text.drop 2 strippedHeader
    | "{-" `isPrefixOf` strippedHeader =
        Data.Text.drop 2 $ Data.Text.dropEnd 2 strippedHeader
    | otherwise = strippedHeader

  where
    strippedHeader = strip header

filePathHeaderToHtml
    :: (FilePath, Header)
    -> Html ()
filePathHeaderToHtml (filePath, header) =
    html_ $ do
        head_ $
            title_ $ toHtml title
        body_ $ do
            h1_ $ toHtml title
            headerToHtml header
  where
    title = Turtle.format fp $ Turtle.basename filePath
