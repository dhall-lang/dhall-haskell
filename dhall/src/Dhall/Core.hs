{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

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
    , Expr(..)

    -- * Normalization
    , alphaNormalize
    , normalize
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
    , freeIn

    -- * Pretty-printing
    , pretty

    -- * Optics
    , subExpressions
    , chunkExprs
    , bindingExprs

    -- * Let-blocks
    , multiLet
    , wrapInLets
    , MultiLet(..)

    -- * Miscellaneous
    , internalError
    , reservedIdentifiers
    , escapeText
    , pathCharacter
    , throws
    , Eval.textShow
    , censorExpression
    , censorText
    ) where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty)
import Dhall.Normalize
import Dhall.Src (Src(..))
import Dhall.Syntax
import Dhall.Pretty.Internal
import Instances.TH.Lift ()
import Lens.Family (over)
import Prelude hiding (succ)

import qualified Control.Exception
import qualified Dhall.Eval    as Eval
import qualified Data.Text

-- | Pretty-print a value
pretty :: Pretty a => a -> Text
pretty = pretty_
{-# INLINE pretty #-}

_ERROR :: String
_ERROR = "\ESC[1;31mError\ESC[0m"

{-| Utility function used to throw internal errors that should never happen
    (in theory) but that are not enforced by the type system
-}
internalError :: Data.Text.Text -> forall b . b
internalError text = error (unlines
    [ _ERROR <> ": Compiler bug                                                        "
    , "                                                                                "
    , "Explanation: This error message means that there is a bug in the Dhall compiler."
    , "You didn't do anything wrong, but if you would like to see this problem fixed   "
    , "then you should report the bug at:                                              "
    , "                                                                                "
    , "https://github.com/dhall-lang/dhall-haskell/issues                              "
    , "                                                                                "
    , "Please include the following text in your bug report:                           "
    , "                                                                                "
    , "```                                                                             "
    , Data.Text.unpack text <> "                                                       "
    , "```                                                                             "
    ] )

{-| Escape a `Text` literal using Dhall's escaping rules

    Note that the result does not include surrounding quotes
-}
escapeText :: Text -> Text
escapeText = escapeText_
{-# INLINE escapeText #-}


{-| Utility used to implement the @--censor@ flag, by:

    * Replacing all `Src` text with spaces
    * Replacing all `Text` literals inside type errors with spaces
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

-- | Utility used to censor `Text` by replacing all characters with a space
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
