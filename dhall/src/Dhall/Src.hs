{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | This module provides the `Src` type used for source spans in error messages

module Dhall.Src
    ( -- * Type
      Src(..)
    ) where

import Control.DeepSeq            (NFData)
import Data.Data                  (Data)
import Data.Text                  (Text)
import GHC.Generics               (Generic)
import Instances.TH.Lift          ()
import Language.Haskell.TH.Syntax (Lift (..))
import Prettyprinter              (Pretty (..))
import Text.Megaparsec            (SourcePos (SourcePos), mkPos, unPos)

import {-# SOURCE #-} qualified Dhall.Util

import qualified Data.Text       as Text
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Printf     as Printf

-- | Source code extract
data Src = Src
    { srcStart :: !SourcePos
    , srcEnd   :: !SourcePos
    , srcText  :: Text -- Text field is intentionally lazy
    } deriving (Data, Eq, Generic, Ord, Show, NFData)


instance Lift Src where
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped (Src (SourcePos a b c) (SourcePos d e f) g) =
        [|| Src (SourcePos a (mkPos b') (mkPos c')) (SourcePos d (mkPos e') (mkPos f')) g ||]
#else
    lift (Src (SourcePos a b c) (SourcePos d e f) g) =
        [| Src (SourcePos a (mkPos b') (mkPos c')) (SourcePos d (mkPos e') (mkPos f')) g |]
#endif
      where
        b' = unPos b
        c' = unPos c
        e' = unPos e
        f' = unPos f


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
