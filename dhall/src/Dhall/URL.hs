{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.URL where

import Data.Text (Text)

import Dhall.Syntax (Directory (..), File (..), Scheme (..), URL (..))

renderComponent :: Text -> Text
renderComponent component = "/" <> component

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
