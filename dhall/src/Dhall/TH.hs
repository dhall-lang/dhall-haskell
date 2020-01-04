{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Template Haskell utilities
module Dhall.TH
    ( -- * Template Haskell
      staticDhallExpression
    , makeHaskellType
    ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty)
import Dhall.Syntax (Expr(..))
import Language.Haskell.TH.Quote (dataToExpQ) -- 7.10 compatibility.

import Language.Haskell.TH.Syntax
    ( Con(..)
    , Dec(..)
    , Exp(..)
    , Q
    , Type(..)
#if MIN_VERSION_template_haskell(2,11,0)
    , Bang(..)
    , SourceStrictness(..)
    , SourceUnpackedness(..)
#else
    , Strict(..)
#endif
    )

import qualified Data.Text                               as Text
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty
import qualified Data.Typeable                           as Typeable
import qualified Dhall
import qualified Dhall.Map
import qualified Dhall.Pretty
import qualified Dhall.Util
import qualified GHC.IO.Encoding
import qualified Numeric.Natural
import qualified System.IO
import qualified Language.Haskell.TH.Syntax              as Syntax

{-| This fully resolves, type checks, and normalizes the expression, so the
    resulting AST is self-contained.

    This can be used to resolve all of an expression’s imports at compile time,
    allowing one to reference Dhall expressions from Haskell without having a
    runtime dependency on the location of Dhall files.

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
staticDhallExpression :: Text -> Q Exp
staticDhallExpression text = do
    Syntax.runIO (GHC.IO.Encoding.setLocaleEncoding System.IO.utf8)

    expression <- Syntax.runIO (Dhall.inputExpr text)

    dataToExpQ (\a -> liftText <$> Typeable.cast a) expression
  where
    -- A workaround for a problem in TemplateHaskell (see
    -- https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable)
    liftText = fmap (AppE (VarE 'Text.pack)) . Syntax.lift . Text.unpack

{-| Convert a Dhall type to a Haskell type that does not require any new
    data declarations
-}
toSimpleHaskellType :: Pretty a => Expr s a -> Q Type
toSimpleHaskellType dhallType =
    case dhallType of
        Bool -> do
            return (ConT ''Bool)

        Double -> do
            return (ConT ''Double)

        Integer -> do
            return (ConT ''Integer)

        Natural -> do
            return (ConT ''Numeric.Natural.Natural)

        Text -> do
            return (ConT ''Text)

        App List dhallElementType -> do
            haskellElementType <- toSimpleHaskellType dhallElementType

            return (AppT (ConT ''[]) haskellElementType)

        App Optional dhallElementType -> do
            haskellElementType <- toSimpleHaskellType dhallElementType

            return (AppT (ConT ''Maybe) haskellElementType)

        _ -> do
            let document =
                    mconcat
                    [ "Dhall.TH.makeHaskellType: Unsupported simple type\n"
                    , "                                                                                \n"
                    , "Explanation: Not all Dhall alternative types can be converted to Haskell        \n"
                    , "constructor types.  Specifically, only the following simple Dhall types are     \n"
                    , "supported as an alternative type or a field of an alternative type:             \n"
                    , "                                                                                \n"
                    , "• ❰Bool❱                                                                        \n"
                    , "• ❰Double❱                                                                      \n"
                    , "• ❰Integer❱                                                                     \n"
                    , "• ❰Natural❱                                                                     \n"
                    , "• ❰Text❱                                                                        \n"
                    , "• ❰List a❱     (where ❰a❱ is also a simple type)                                \n"
                    , "• ❰Optional a❱ (where ❰a❱ is also a simple type)                                \n"
                    , "                                                                                \n"
                    , "The Haskell datatype generation logic encountered the following complex         \n"
                    , "Dhall type:                                                                     \n"
                    , "                                                                                \n"
                    , " " <> Dhall.Util.insert dhallType <> "\n"
                    , "                                                                                \n"
                    , "... where a simpler type was expected."
                    ]

            let message = Pretty.renderString (Dhall.Pretty.layout document)

            fail message

-- | Convert a Dhall type to the corresponding Haskell constructor type
toConstructor :: Pretty a => (Text, Maybe (Expr s a)) -> Q Con
toConstructor (constructorName, maybeAlternativeType) = do
    let name = Syntax.mkName (Text.unpack constructorName)

#if MIN_VERSION_template_haskell(2,11,0)
    let bang = Bang NoSourceUnpackedness NoSourceStrictness
#else
    let bang = NotStrict
#endif

    case maybeAlternativeType of
        Just (Record kts) -> do
            let process (key, dhallFieldType) = do
                    haskellFieldType <- toSimpleHaskellType dhallFieldType

                    return (Syntax.mkName (Text.unpack key), bang, haskellFieldType)

            varBangTypes <- traverse process (Dhall.Map.toList kts)

            return (RecC name varBangTypes)

        Just dhallAlternativeType -> do
            haskellAlternativeType <- toSimpleHaskellType dhallAlternativeType

            return (NormalC name [ (bang, haskellAlternativeType) ])

        Nothing -> do
            return (NormalC name [])

-- | Generate a Haskell datatype declaration from a Dhall type
--
-- This comes in handy if you need to keep a Dhall type and Haskell type in
-- sync.  You make the Dhall type the source of truth and use Template Haskell
-- to generate the matching Haskell type declaration from the Dhall type.
--
-- For example, this Template Haskell splice:
--
-- > Dhall.TH.makeHaskellType "T" "< A : { x : Bool } | B >"
--
-- ... generates this Haskell code:
--
-- > data T = A {x :: GHC.Types.Bool} | B
--
-- To add any desired instances (such as `Dhall.FromDhall`/`Dhall.ToDhall`),
-- you can use the `StandaloneDeriving` language extension, like this:
--
-- > {-# LANGUAGE DeriveAnyClass     #-}
-- > {-# LANGUAGE DeriveGeneric      #-}
-- > {-# LANGUAGE OverloadedStrings  #-}
-- > {-# LANGUAGE StandaloneDeriving #-}
-- > {-# LANGUAGE TemplateHaskell    #-}
-- >
-- > Dhall.TH.makeHaskellType "T" "< A : { x : Bool } | B >"
-- > 
-- > deriving instance Generic   T
-- > deriving instance FromDhall T
--
-- Carefully note that the generated Haskell type only corresponds to the Dhall
-- type when using the `Dhall.Smart` setting for the
-- `Dhall.singletonConstructors` field of `InterpretOptions`.  That setting
-- will eventually become the default, but until then you will need to
-- explicitly use the setting like this:
--
-- > let interpretOptions =
-- >         Dhall.defaultInterpretOptions
-- >             { Dhall.singletonConstructors = Dhall.Smart
-- >             }
-- >
-- > let decoder = Dhall.autoWith interpretOptions

makeHaskellType :: Text -> Text -> Q [Dec]
makeHaskellType typeName text = do
    Syntax.runIO (GHC.IO.Encoding.setLocaleEncoding System.IO.utf8)

    expression <- Syntax.runIO (Dhall.inputExpr text)

    case expression of
        Union kts -> do
            let name = Syntax.mkName (Text.unpack typeName)

            constructors <- traverse toConstructor (Dhall.Map.toList kts )

            let declaration = DataD [] name []
#if MIN_VERSION_template_haskell(2,11,0)
                    Nothing
#else
#endif
                    constructors []

            return [ declaration ]

        _ -> do
            let document =
                    mconcat
                    [ "Dhall.TH.makeHaskellType: Unsupported Dhall type\n"
                    , "                                                                                \n"
                    , "Explanation: Not all Dhall types can be converted to Haskell datatype           \n"
                    , "declarations.  Specifically, only union types can be converted to Haskell types.\n"
                    , "                                                                                \n"
                    , "For example, this is a valid Dhall type:                                        \n"
                    , "                                                                                \n"
                    , "                                                                                \n"
                    , "    ┌─────────────────────────────────────────────────────────┐                 \n"
                    , "    │ Dhall.TH.makeHaskellType \"T\" \"< A : { x : Bool } | B >\" │                 \n"
                    , "    └─────────────────────────────────────────────────────────┘                 \n"
                    , "                                                                                \n"
                    , "                                                                                \n"
                    , "... which corresponds to this Haskell type declaration:                         \n"
                    , "                                                                                \n"
                    , "                                                                                \n"
                    , "    ┌──────────────────────────────────────┐                                    \n"
                    , "    │ data T = A {x :: GHC.Types.Bool} | B │                                    \n"
                    , "    └──────────────────────────────────────┘                                    \n"
                    , "                                                                                \n"
                    , "                                                                                \n"
                    , "... but the following Dhall type is rejected due to being a bare record type:   \n"
                    , "                                                                                \n"
                    , "                                                                                \n"
                    , "    ┌─────────────────────────────────────────────┐                             \n"
                    , "    │ Dhall.TH.makeHaskellType \"T\" \"{ x : Bool }\" │  Not valid due to not being \n"
                    , "    └─────────────────────────────────────────────┘  wrapped in a union         \n"
                    , "                                                                                \n"
                    , "                                                                                \n"
                    , "The Haskell datatype generation logic encountered the following Dhall type:     \n"
                    , "                                                                                \n"
                    , " " <> Dhall.Util.insert expression <> "\n"
                    , "                                                                                \n"
                    , "... which is not a union type."
                    ]

            let message = Pretty.renderString (Dhall.Pretty.layout document)

            fail message
