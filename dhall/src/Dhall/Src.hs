{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | This module provides the `Src` type used for source spans in error messages

module Dhall.Src
    ( -- * Type
      Src(..)
    ) where

import Data.Data (Data)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc  (Pretty (..))
import GHC.Generics (Generic)
import Text.Megaparsec (SourcePos)

import {-# SOURCE #-} qualified Dhall.Util

import qualified Data.Text       as Text
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Printf     as Printf

-- | Source code extract
data Src = Src !SourcePos !SourcePos Text
  -- Text field is intentionally lazy
  deriving (Data, Eq, Generic, Ord, Show)

instance Pretty Src where
    pretty (Src begin _ text) =
            pretty (Dhall.Util.snip numberedLines)
        <>  "\n"
        <>  pretty (Megaparsec.sourcePosPretty begin)
      where
        prefix = Text.replicate (n - 1) " "
          where
            n = Megaparsec.unPos (Megaparsec.sourceColumn begin)

        ls = Text.lines (prefix <> text)

        numberOfLines = length ls

        minimumNumber =
            Megaparsec.unPos (Megaparsec.sourceLine begin)

        maximumNumber = minimumNumber + numberOfLines - 1

        numberWidth :: Int
        numberWidth =
            truncate (logBase (10 :: Double) (fromIntegral maximumNumber)) + 1

        adapt n line = Text.pack outputString
          where
            inputString = Text.unpack line

            outputString =
                Printf.printf ("%" <> show numberWidth <> "d│ %s") n inputString

        numberedLines = Text.unlines (zipWith adapt [minimumNumber..] ls)
