module Dhall.Parser.Token where

import Data.HashSet (HashSet)
import Data.Text (Text)

reservedIdentifiers :: HashSet Text

pathCharacter :: Char -> Bool