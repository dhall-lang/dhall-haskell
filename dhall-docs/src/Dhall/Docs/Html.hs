{-| This module expors all functions used to generate HTML from a dhall package
-}

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Dhall.Docs.Html (headerToHtml) where

import Data.Text
import Dhall.Parser (Header (..))
import Lucid

-- | Takes a `Header` and generates its `Html`
headerToHtml :: Header -> Html ()
headerToHtml (Header header) = p_ $ toHtml removeComments
  where
    strippedHeader = strip header

    removeComments :: Text
    removeComments
        | "--" `isPrefixOf` strippedHeader = Data.Text.drop 2 strippedHeader
        | "{-" `isPrefixOf` strippedHeader =
            Data.Text.drop 2 $ Data.Text.dropEnd 2 strippedHeader
        | otherwise = strippedHeader
