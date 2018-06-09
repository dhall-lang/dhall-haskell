{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Dhall.Parser.Combinators where


import           Control.Applicative        (Alternative (..), liftA2, optional)
import           Control.Exception          (Exception)
import           Control.Monad              (MonadPlus)
import           Data.ByteArray.Encoding    (Base (..))
import           Data.Data                  (Data)
import           Data.Functor               (void)
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import           Data.Scientific            (Scientific)
import           Data.Semigroup             (Semigroup (..))
import           Data.Sequence              (ViewL (..))
import           Data.Set                   (Set)
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Dhall.Core
import           Formatting.Buildable       (Buildable (..))
import           Numeric.Natural            (Natural)
import           Prelude                    hiding (const, pi)
import           Text.Parser.Combinators    (choice, try, (<?>))
import           Text.Parser.Token          (TokenParsing (..))

import qualified Control.Monad
import qualified Crypto.Hash
import qualified Data.ByteArray.Encoding
import qualified Data.ByteString
import qualified Data.Char
import qualified Data.HashMap.Strict.InsOrd
import qualified Data.HashSet
import qualified Data.List
import qualified Data.List.NonEmpty
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Text.Megaparsec
import qualified Text.Megaparsec.Char
import qualified Text.Parser.Char
import qualified Text.Parser.Combinators
import qualified Text.Parser.Token
import qualified Text.Parser.Token.Style


