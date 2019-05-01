{-# LANGUAGE TemplateHaskell #-}

{-| This module provides `staticDhallExpression` which can be used to resolve
    all of an expression’s imports at compile time, allowing one to reference
    Dhall expressions from Haskell without having a runtime dependency on the
    location of Dhall files.

    For example, given a file @".\/Some\/Type.dhall"@ containing

    > < This : Natural | Other : ../Other/Type.dhall >

    ... rather than duplicating the AST manually in a Haskell `Type`, you can
    do:

    > Dhall.Type
    > (\case
    >     UnionLit "This" _ _  -> ...
    >     UnionLit "Other" _ _ -> ...)
    > $(staticDhallExpression "./Some/Type.dhall")

    This would create the Dhall Expr AST from the @".\/Some\/Type.dhall"@ file
    at compile time with all imports resolved, making it easy to keep your Dhall
    configs and Haskell interpreters in sync.
-}
module Dhall.TH
    ( -- * Template Haskell
     staticDhallExpression
    ) where

import Data.Typeable (cast)
import Language.Haskell.TH.Quote (dataToExpQ) -- 7.10 compatibility.
import Language.Haskell.TH.Syntax (Q(..), Exp(..), runIO, lift)

import qualified Data.Text as Text
import qualified Dhall
import qualified GHC.IO.Encoding
import qualified System.IO

-- | This fully resolves, type checks, and normalizes the expression, so the
--   resulting AST is self-contained.
staticDhallExpression :: Text.Text -> Q Exp
staticDhallExpression text = do
    runIO (GHC.IO.Encoding.setLocaleEncoding System.IO.utf8)
    expression <- runIO (Dhall.inputExpr text)
    dataToExpQ (\a -> liftText <$> cast a) expression
  where
    -- A workaround for a problem in TemplateHaskell (see
    -- https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable)
    liftText = fmap (AppE (VarE 'Text.pack)) . lift . Text.unpack
