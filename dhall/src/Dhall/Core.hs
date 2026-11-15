{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

{-| This module contains the core calculus for the Dhall language.

    Dhall is essentially a fork of the @morte@ compiler but with more built-in
    functionality, better error messages, and Haskell integration
-}

module Dhall.Core (
    -- * Syntax
      Const(..)
    , Directory(..)
    , File(..)
    , FilePrefix(..)
    , Import(..)
    , ImportHashed(..)
    , ImportMode(..)
    , ImportType(..)
    , URL(..)
    , Scheme(..)
    , DhallDouble(..)
    , Var(..)
    , Binding(..)
    , makeBinding
    , Chunks(..)
    , PreferAnnotation(..)
    , RecordField(..)
    , makeRecordField
    , FunctionBinding(..)
    , makeFunctionBinding
    , FieldSelection (..)
    , makeFieldSelection
    , WithComponent (..)
    , Expr(..)

    -- * Normalization
    , alphaNormalize
    , normalize
    , normalizeToGraph
    , normalizeWithConfigToGraph
    , normalizeToExprLegacy
    , CanonicalGraph (..)
    , PresentationGraph (..)
    , toPresentationGraph
    , presentationExpr
    , alphaCanonical
    , normalizeGraph
    , normalizeGraphWith
    , normalizeWith
    , normalizeWithM
    , Normalizer
    , NormalizerM
    , ReifiedNormalizer (..)
    , judgmentallyEqual
    , subst
    , shift
    , isNormalized
    , isNormalizedWith
    , denote
    , renote
    , shallowDenote
    , cse
    , cseWith
    , cseGraph
    , CseOptions(..)
    , defaultCseOptions
    , freeIn

    -- * Pretty-printing
    , pretty

    -- * Optics
    , subExpressions
    , subExpressionsWith
    , chunkExprs
    , bindingExprs
    , recordFieldExprs
    , functionBindingExprs

    -- * Hash-consed core
    , DAG.CoreGraph
    , DAG.SharingMode(..)
    , DAG.fromExpr
    , DAG.toSharedExpr
    , DAG.prettyGraph
    , DAG.graphMetrics
    , DAG.GraphMetrics(..)
    , DAG.alphaNormalizeGraph
    , hashExpressionGraph
    -- * Experimental DAG CBOR (not the default semantic hash)
    , DAGCBOR.encodeGraph
    , DAGCBOR.decodeGraph
    -- * Identified evaluation
    , Eval.EvalConfig(..)
    , Eval.EvalStats(..)
    , Eval.defaultEvalConfig
    , Eval.cacheOnConfig
    , Eval.cacheOffConfig
    , Eval.withEvalRun
    , Eval.withEvalRunIO
    , Eval.readEvalStats
    , Eval.readEvalStatsIO
    , Eval.normalizeWithConfig
    , Eval.normalizeWithStats
    , Eval.evalInRun
    , Eval.quoteInRun
    , Eval.quoteGraph
    , Eval.valId
    , Eval.Environment(Empty)
    , Eval.Names(EmptyNames)

    -- * Let-blocks
    , multiLet
    , wrapInLets
    , MultiLet(..)

    -- * Miscellaneous
    , internalError
    , reservedIdentifiers
    , reservedFunctionNames
    , fixedSymbolIdentifiers
    , escapeText
    , pathCharacter
    , throws
    , Eval.textShow
    , censorExpression
    , censorText
    ) where

import Control.Exception      (Exception)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text              (Text)
import Data.Void              (Void)
import Dhall.Core.CSE
    ( CseOptions (..)
    , cse
    , cseGraph
    , cseWith
    , defaultCseOptions
    )
import Dhall.Core.NormalForm
    ( CanonicalGraph (..)
    , PresentationGraph (..)
    , alphaCanonical
    , normalizeToExprLegacy
    , normalizeToGraph
    , normalizeWithConfigToGraph
    , presentationExpr
    , toPresentationGraph
    )
import Dhall.Normalize
import Dhall.Pretty.Internal
import Dhall.Src              (Src (..))
import Dhall.Syntax
import Instances.TH.Lift      ()
import Lens.Micro             (over)
import Prettyprinter          (Pretty)

import qualified Control.Exception
import qualified Data.Text
import qualified Dhall.Core.DAG      as DAG
import qualified Dhall.Core.DAG.CBOR as DAGCBOR
import qualified Dhall.Crypto
import qualified Dhall.Eval          as Eval

-- | Pretty-print a value
pretty :: Pretty a => a -> Text
pretty = pretty_
{-# INLINE pretty #-}

-- | Hash-consed beta-normal form. 'normalize' is this graph rebuilt as a
-- physically shared 'Expr'.
normalizeGraph :: Eq a => Expr s a -> DAG.CoreGraph a
normalizeGraph = Eval.normalizeGraph

-- | Same as 'normalizeGraph' with intern sharing enabled or disabled.
normalizeGraphWith :: Eq a => DAG.SharingMode -> Expr s a -> DAG.CoreGraph a
normalizeGraphWith = Eval.normalizeGraphWith

-- | SHA-256 of the *legacy tree* CBOR of a graph. Same bytes as hashing
-- 'toSharedExpr'; DAG CBOR is experimental and is not used here.
hashExpressionGraph :: DAG.CoreGraph Void -> Dhall.Crypto.SHA256Digest
hashExpressionGraph = DAGCBOR.hashGraphLegacy

{-| Escape a `Data.Text.Text` literal using Dhall's escaping rules

    Note that the result does not include surrounding quotes
-}
escapeText :: Text -> Text
escapeText = escapeText_
{-# INLINE escapeText #-}


{-| Utility used to implement the @--censor@ flag, by:

    * Replacing all `Src` text with spaces
    * Replacing all `Dhall.Syntax.Text` literals inside type errors with spaces
-}
censorExpression :: Expr Src a -> Expr Src a
censorExpression (TextLit chunks) = TextLit (censorChunks chunks)
censorExpression (Note src     e) = Note (censorSrc src) (censorExpression e)
censorExpression  e               = over subExpressions censorExpression e

censorChunks :: Chunks Src a -> Chunks Src a
censorChunks (Chunks xys z) = Chunks xys' z'
  where
    z' = censorText z

    xys' = [ (censorText x, censorExpression y) | (x, y) <- xys ]

{-| Utility used to censor `Data.Text.Text` by replacing all characters with a
    space
-}
censorText :: Text -> Text
censorText = Data.Text.map (\_ -> ' ')

censorSrc :: Src -> Src
censorSrc (Src { srcText = oldText, .. }) = Src { srcText = newText, .. }
  where
    newText = censorText oldText

{-| Convenience utility for converting `Either`-based exceptions to `IO`-based
    exceptions
-}
throws :: (Exception e, MonadIO io) => Either e a -> io a
throws (Left  e) = liftIO (Control.Exception.throwIO e)
throws (Right r) = return r
{-# INLINABLE throws #-}

{- $setup
>>> import qualified Codec.Serialise
>>> import qualified Dhall.Binary
>>> import Data.SpecialValues
>>> import Test.QuickCheck (Arbitrary(..), oneof, elements)
>>> :{
  instance Arbitrary DhallDouble where
    arbitrary = fmap DhallDouble (oneof [ arbitrary, elements specialValues ])
:}
-}
