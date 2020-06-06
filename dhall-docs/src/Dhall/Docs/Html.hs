{-| This module expors all functions used to generate HTML from a dhall package
-}

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Dhall.Docs.Html () where

import Dhall.Parser (ParseError (..))
import Lucid

