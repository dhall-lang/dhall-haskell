{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.URL where

import Data.Text (Text)

import Dhall.Core
    ( Scheme(..)
    , URL(..)
    , File(..)
    , Directory(..)
    )

import qualified Network.URI.Encode as URI.Encode

renderComponent :: Text -> Text
renderComponent component = "/" <> URI.Encode.encodeText component

renderQuery :: Text -> Text
renderQuery query = "?" <> query

renderURL :: URL -> Text
renderURL url =
        schemeText
    <>  authority
    <>  pathText
    <>  queryText
  where
    URL {..} = url

    File {..} = path

    Directory {..} = directory

    schemeText = case scheme of
        HTTP  -> "http://"
        HTTPS -> "https://"

    pathText =
            foldMap renderComponent (reverse components)
        <>  renderComponent file

    queryText = foldMap renderQuery query
