{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Template Haskell utilities
module Dhall.TH
    ( -- * Template Haskell
      staticDhallExpression
    , makeHaskellTypeFromUnion
    , makeHaskellTypes
    , HaskellType(..)
    ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty)
import Dhall.Syntax (Expr(..))
import Dhall (FromDhall, ToDhall)
import GHC.Generics (Generic)
import Language.Haskell.TH.Quote (dataToExpQ) -- 7.10 compatibility.

import Language.Haskell.TH.Syntax
    ( Con(..)
    , Dec(..)
    , Exp(..)
    , Q
    , Type(..)
    , Bang(..)
    , SourceStrictness(..)
    , SourceUnpackedness(..)
#if MIN_VERSION_template_haskell(2,12,0)
    , DerivClause(..)
    , DerivStrategy(..)
#else
    , Pred
#endif
    )

import qualified Data.List                               as List
import qualified Data.Text                               as Text
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty
import qualified Data.Typeable                           as Typeable
import qualified Dhall
import qualified Dhall.Core                              as Core
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
    data declarations beyond the data declarations supplied as the first
    argument
-}
toNestedHaskellType
    :: (Eq a, Pretty a)
    => [HaskellType (Expr s a)]
    -- ^ All Dhall-derived data declarations
    --
    -- Used to replace complex types with references to one of these
    -- data declarations when the types match
    -> Expr s a
    -- ^ Dhall expression to convert to a simple Haskell type
    -> Q Type
toNestedHaskellType haskellTypes = loop
  where
    loop dhallType = case dhallType of
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
            haskellElementType <- loop dhallElementType

            return (AppT (ConT ''[]) haskellElementType)

        App Optional dhallElementType -> do
            haskellElementType <- loop dhallElementType

            return (AppT (ConT ''Maybe) haskellElementType)

        _   | Just haskellType <- List.find predicate haskellTypes -> do
                let name = Syntax.mkName (Text.unpack (typeName haskellType))

                return (ConT name) 
            | otherwise -> do
            let document =
                    mconcat
                    [ "Unsupported nested type\n"
                    , "                                                                                \n"
                    , "Explanation: Not all Dhall types can be nested within Haskell datatype          \n"
                    , "declarations.  Specifically, only the following simple Dhall types are supported\n"
                    , "as a nested type inside of a data declaration:                                  \n"
                    , "                                                                                \n"
                    , "• ❰Bool❱                                                                        \n"
                    , "• ❰Double❱                                                                      \n"
                    , "• ❰Integer❱                                                                     \n"
                    , "• ❰Natural❱                                                                     \n"
                    , "• ❰Text❱                                                                        \n"
                    , "• ❰List a❱     (where ❰a❱ is also a valid nested type)                          \n"
                    , "• ❰Optional a❱ (where ❰a❱ is also a valid nested type)                          \n"
                    , "• Another matching datatype declaration                                         \n"
                    , "                                                                                \n"
                    , "The Haskell datatype generation logic encountered the following Dhall type:     \n"
                    , "                                                                                \n"
                    , " " <> Dhall.Util.insert dhallType <> "\n"
                    , "                                                                                \n"
                    , "... which did not fit any of the above criteria."
                    ]

            let message = Pretty.renderString (Dhall.Pretty.layout document)

            fail message
          where
            predicate haskellType =
                Core.judgmentallyEqual (code haskellType) dhallType

#if MIN_VERSION_template_haskell(2,12,0)
derivingClauses :: [DerivClause]
derivingClauses =
    [ DerivClause (Just StockStrategy) [ ConT ''Generic ]
    , DerivClause (Just AnyclassStrategy) [ ConT ''FromDhall, ConT ''ToDhall ]
    ]
#else
derivingClauses :: [Pred]
derivingClauses = [ ConT ''Generic, ConT ''FromDhall, ConT ''ToDhall ]
#endif

-- | Convert a Dhall type to the corresponding Haskell datatype declaration
toDeclaration
    :: (Eq a, Pretty a)
    => [HaskellType (Expr s a)]
    -> HaskellType (Expr s a)
    -> Q Dec
toDeclaration haskellTypes MultipleConstructors{..} = do
    case code of
        Union kts -> do
            let name = Syntax.mkName (Text.unpack typeName)

            constructors <- traverse (toConstructor haskellTypes) (Dhall.Map.toList kts )

            return (DataD [] name [] Nothing constructors derivingClauses)

        _ -> do
            let document =
                    mconcat
                    [ "Dhall.TH.makeHaskellTypes: Not a union type\n"
                    , "                                                                                \n"
                    , "Explanation: This function expects the ❰code❱ field of ❰MultipleConstructors❱ to\n"
                    , "evaluate to a union type.                                                       \n"
                    , "                                                                                \n"
                    , "For example, this is a valid Dhall union type that this function would accept:  \n"
                    , "                                                                                \n"
                    , "                                                                                \n"
                    , "    ┌──────────────────────────────────────────────────────────────────┐        \n"
                    , "    │ Dhall.TH.makeHaskellTypes (MultipleConstructors \"T\" \"< A | B >\") │        \n"
                    , "    └──────────────────────────────────────────────────────────────────┘        \n"
                    , "                                                                                \n"
                    , "                                                                                \n"
                    , "... which corresponds to this Haskell type declaration:                         \n"
                    , "                                                                                \n"
                    , "                                                                                \n"
                    , "    ┌────────────────┐                                                          \n"
                    , "    │ data T = A | B │                                                          \n"
                    , "    └────────────────┘                                                          \n"
                    , "                                                                                \n"
                    , "                                                                                \n"
                    , "... but the following Dhall type is rejected due to being a bare record type:   \n"
                    , "                                                                                \n"
                    , "                                                                                \n"
                    , "    ┌──────────────────────────────────────────────┐                            \n"
                    , "    │ Dhall.TH.makeHaskellTypes \"T\" \"{ x : Bool }\" │  Not valid                 \n"
                    , "    └──────────────────────────────────────────────┘                            \n"
                    , "                                                                                \n"
                    , "                                                                                \n"
                    , "The Haskell datatype generation logic encountered the following Dhall type:     \n"
                    , "                                                                                \n"
                    , " " <> Dhall.Util.insert code <> "\n"
                    , "                                                                                \n"
                    , "... which is not a union type."
                    ]

            let message = Pretty.renderString (Dhall.Pretty.layout document)

            fail message
toDeclaration haskellTypes SingleConstructor{..} = do
    let name = Syntax.mkName (Text.unpack typeName)

    constructor <- toConstructor haskellTypes (constructorName, Just code)

    return (DataD [] name [] Nothing [constructor] derivingClauses)

-- | Convert a Dhall type to the corresponding Haskell constructor
toConstructor
    :: (Eq a, Pretty a)
    => [HaskellType (Expr s a)]
    -> (Text, Maybe (Expr s a))
    -- ^ @(constructorName, fieldType)@
    -> Q Con
toConstructor haskellTypes (constructorName, maybeAlternativeType) = do
    let name = Syntax.mkName (Text.unpack constructorName)

    let bang = Bang NoSourceUnpackedness NoSourceStrictness

    case maybeAlternativeType of
        Just (Record kts) -> do
            let process (key, dhallFieldType) = do
                    haskellFieldType <- toNestedHaskellType haskellTypes dhallFieldType

                    return (Syntax.mkName (Text.unpack key), bang, haskellFieldType)

            varBangTypes <- traverse process (Dhall.Map.toList kts)

            return (RecC name varBangTypes)

        Just dhallAlternativeType -> do
            haskellAlternativeType <- toNestedHaskellType haskellTypes dhallAlternativeType

            return (NormalC name [ (bang, haskellAlternativeType) ])

        Nothing -> do
            return (NormalC name [])

-- | Generate a Haskell datatype declaration from a Dhall union type where
-- each union alternative corresponds to a Haskell constructor
--
-- For example, this Template Haskell splice:
--
-- > Dhall.TH.makeHaskellTypeFromUnion "T" "< A : { x : Bool } | B >"
--
-- ... generates this Haskell code:
--
-- > data T = A {x :: GHC.Types.Bool} | B
--
-- This is a special case of `Dhall.TH.makeHaskellTypes`:
--
-- > makeHaskellTypeFromUnion typeName code =
-- >     makeHaskellTypes [ MultipleConstructors{..} ]
makeHaskellTypeFromUnion
    :: Text
    -- ^ Name of the generated Haskell type
    -> Text
    -- ^ Dhall code that evaluates to a union type
    -> Q [Dec]
makeHaskellTypeFromUnion typeName code =
    makeHaskellTypes [ MultipleConstructors{..} ]

-- | Used by `makeHaskellTypes` to specify how to generate Haskell types
data HaskellType code
    -- | Generate a Haskell type with more than one constructor from a Dhall
    -- union type
    = MultipleConstructors
        { typeName :: Text
        -- ^ Name of the generated Haskell type
        , code :: code
        -- ^ Dhall code that evaluates to a union type
        }
    -- | Generate a Haskell type with one constructor from any Dhall type
    --
    -- To generate a constructor with multiple named fields, supply a Dhall
    -- record type.  This does not support more than one anonymous field.
    | SingleConstructor
        { typeName :: Text
        -- ^ Name of the generated Haskell type
        , constructorName :: Text
        -- ^ Name of the constructor
        , code :: code
        -- ^ Dhall code that evaluates to a type
        }
    deriving (Functor, Foldable, Traversable)

-- | Generate a Haskell datatype declaration with one constructor from a Dhall
-- type
--
-- This comes in handy if you need to keep Dhall types and Haskell types in
-- sync.  You make the Dhall types the source of truth and use Template Haskell
-- to generate the matching Haskell type declarations from the Dhall types.
--
-- For example, given this Dhall code:
--
-- > -- ./Department.dhall
-- > < Sales | Engineering | Marketing >
--
-- > -- ./Employee.dhall
-- > { name : Text, department : ./Department.dhall }
--
-- ... this Template Haskell splice:
--
-- > {-# LANGUAGE DeriveAnyClass     #-}
-- > {-# LANGUAGE DeriveGeneric      #-}
-- > {-# LANGUAGE DerivingStrategies #-}
-- > {-# LANGUAGE OverloadedStrings  #-}
-- > {-# LANGUAGE TemplateHaskell    #-}
-- >
-- > Dhall.TH.makeHaskellTypes
-- >     [ MultipleConstructors "Department" "./tests/th/Department.dhall"
-- >     , SingleConstructor "Employee" "MakeEmployee" "./tests/th/Employee.dhall"
-- >     ]
--
-- ... generates this Haskell code:
--
-- > data Department = Engineering | Marketing | Sales
-- >   deriving stock (GHC.Generics.Generic)
-- >   deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)
-- >
-- > data Employee
-- >   = MakeEmployee {department :: Department,
-- >                   name :: Data.Text.Internal.Text}
-- >   deriving stock (GHC.Generics.Generic)
-- >   deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)
--
-- Carefully note that the conversion makes a best-effort attempt to
-- auto-detect when a Dhall type (like @./Employee.dhall@) refers to another
-- Dhall type (like @./Department.dhall@) and replaces that reference with the
-- corresponding Haskell type.
--
-- This Template Haskell splice requires you to enable the following extensions:
--
-- * @DeriveGeneric@
-- * @DerivingAnyClass@
-- * @DerivingStrategies@
--
-- By default, the generated types only derive `GHC.Generics.Generic`,
-- `Dhall.FromDhall`, and `Dhall.ToDhall`.  To add any desired instances (such
-- as `Eq`\/`Ord`\/`Show`), you can use the `StandaloneDeriving` language
-- extension, like this:
--
-- > {-# LANGUAGE DeriveAnyClass     #-}
-- > {-# LANGUAGE DeriveGeneric      #-}
-- > {-# LANGUAGE DerivingStrategies #-}
-- > {-# LANGUAGE OverloadedStrings  #-}
-- > {-# LANGUAGE StandaloneDeriving #-}
-- > {-# LANGUAGE TemplateHaskell    #-}
-- >
-- > Dhall.TH.makeHaskellTypes
-- >     [ MultipleConstructors "Department" "./tests/th/Department.dhall"
-- >     , SingleConstructor "Employee" "MakeEmployee" "./tests/th/Employee.dhall"
-- >     ]
-- >
-- > deriving instance Eq   Department
-- > deriving instance Ord  Department
-- > deriving instance Show Department
-- >
-- > deriving instance Eq   Employee
-- > deriving instance Ord  Employee
-- > deriving instance Show Employee
makeHaskellTypes :: [HaskellType Text] -> Q [Dec]
makeHaskellTypes haskellTypes = do
    Syntax.runIO (GHC.IO.Encoding.setLocaleEncoding System.IO.utf8)

    haskellTypes' <- traverse (traverse (Syntax.runIO . Dhall.inputExpr)) haskellTypes

    traverse (toDeclaration haskellTypes') haskellTypes'
