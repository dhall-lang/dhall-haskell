{-# LANGUAGE OverloadedStrings #-}

module Dhall.Toml
    ( tomlToDhall
    , dhallToToml
    ) where

import Prelude hiding (putStr, putStrLn)

import Data.Text
import Data.Text.IO (putStrLn)
import Toml (TomlCodec, (.=))
import qualified Toml

data SomeTable = SomeTable
    { boolItem :: Bool
    , textItem  :: Text
    }

someTableCodec :: TomlCodec SomeTable
someTableCodec = SomeTable
    <$> Toml.bool "bool-item" .= boolItem
    <*> Toml.text "text-item" .= textItem

dhallToToml :: IO ()
dhallToToml = putStrLn $ Toml.encode
    someTableCodec
    (SomeTable
        { boolItem = True
        , textItem = "hello"
        })

tomlToDhall :: IO ()
tomlToDhall = putStrLn "not implemented"

