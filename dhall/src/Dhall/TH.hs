{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Template Haskell utilities
module Dhall.TH
    ( -- * Embedding Dhall in Haskell
      staticDhallExpression
    , dhall
      -- * Generating Haskell from Dhall expressions
    , makeHaskellTypeFromUnion
    , makeHaskellTypes
    , makeHaskellTypesWith
    , HaskellType(..)
    , GenerateOptions(..)
    , defaultGenerateOptions
    ) where

import Data.Bifunctor            (first)
import Data.Text                 (Text)
import Dhall                     (FromDhall, ToDhall)
import Dhall.Syntax              (Expr (..), FunctionBinding (..), Var (..))
import GHC.Generics              (Generic)
import Language.Haskell.TH.Quote (QuasiQuoter (..), dataToExpQ)
import Prettyprinter             (Pretty)

import Language.Haskell.TH.Syntax
    ( Bang (..)
    , Body (..)
    , Con (..)
    , Dec (..)
    , Exp (..)
    , Match (..)
    , Pat (..)
    , Q
    , SourceStrictness (..)
    , SourceUnpackedness (..)
    , Type (..)
    )

import Language.Haskell.TH.Syntax (DerivClause (..), DerivStrategy (..))

import qualified Data.List                   as List
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import qualified Data.Text                   as Text
import qualified Data.Time                   as Time
import qualified Data.Typeable               as Typeable
import qualified Dhall
import qualified Dhall.Core                  as Core
import qualified Dhall.Map
import qualified Dhall.Pretty
import qualified Dhall.Util
import qualified GHC.IO.Encoding
import qualified Language.Haskell.TH.Syntax  as TH
import qualified Numeric.Natural
import qualified Prettyprinter.Render.String as Pretty
import qualified System.IO


{-| This fully resolves, type checks, and normalizes the expression, so the
    resulting AST is self-contained.

    This can be used to resolve all of an expression’s imports at compile time,
    allowing one to reference Dhall expressions from Haskell without having a
    runtime dependency on the location of Dhall files.

    For example, given a file @".\/Some\/Type.dhall"@ containing

    > < This : Natural | Other : ../Other/Type.dhall >

    ... rather than duplicating the AST manually in a Haskell `Dhall.Type`, you
    can do:

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
    TH.runIO (GHC.IO.Encoding.setLocaleEncoding System.IO.utf8)

    expression <- TH.runIO (Dhall.inputExpr text)

    dataToExpQ (fmap liftText . Typeable.cast) expression
  where
    -- A workaround for a problem in TemplateHaskell (see
    -- https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable)
    liftText = fmap (AppE (VarE 'Text.pack)) . TH.lift . Text.unpack

{-| A quasi-quoter for Dhall expressions.

    This quoter is build on top of 'staticDhallExpression'. Therefore consult the
    documentation of that function for further information.

    This quoter is meant to be used in expression context only; Other contexts
    like pattern contexts or declaration contexts are not supported and will
    result in an error.
-}
dhall :: QuasiQuoter
dhall = QuasiQuoter
    { quoteExp = staticDhallExpression . Text.pack
    , quotePat = const $ error "dhall quasi-quoter: Quoting patterns is not supported!"
    , quoteType = const $ error "dhall quasi-quoter: Quoting types is not supported!"
    , quoteDec = const $ error "dhall quasi-quoter: Quoting declarations is not supported!"
    }

{-| Convert a Dhall type to a Haskell type that does not require any new
    data declarations beyond the data declarations supplied as the first
    argument
-}
toNestedHaskellType
    :: (Eq a, Pretty a)
    => [Var]
    -> [HaskellType (Expr s a)]
    -- ^ All Dhall-derived data declarations
    --
    -- Used to replace complex types with references to one of these
    -- data declarations when the types match
    -> Expr s a
    -- ^ Dhall expression to convert to a simple Haskell type
    -> Q Type
toNestedHaskellType typeParams haskellTypes = loop
  where
    predicate dhallType haskellType = Core.judgmentallyEqual (code haskellType) dhallType

    document dhallType =
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
      , "• ❰Date❱                                                                        \n"
      , "• ❰TimeOfDay❱                                                                   \n"
      , "• ❰TimeZone❱                                                                    \n"
      , "• ❰List a❱     (where ❰a❱ is also a valid nested type)                          \n"
      , "• ❰Optional a❱ (where ❰a❱ is also a valid nested type)                          \n"
      , "• Another matching datatype declaration                                         \n"
      , "• A bound type variable                                                         \n"
      , "                                                                                \n"
      , "The Haskell datatype generation logic encountered the following Dhall type:     \n"
      , "                                                                                \n"
      , " " <> Dhall.Util.insert dhallType <> "\n"
      , "                                                                                \n"
      , "... which did not fit any of the above criteria."
      ]

    message dhallType = Pretty.renderString (Dhall.Pretty.layout (document dhallType))

    loop dhallType = case dhallType of
        Bool ->
            return (ConT ''Bool)

        Double ->
            return (ConT ''Double)

        Integer ->
            return (ConT ''Integer)

        Natural ->
            return (ConT ''Numeric.Natural.Natural)

        Text ->
            return (ConT ''Text)

        Date ->
            return (ConT ''Time.Day)

        Time ->
            return (ConT ''Time.TimeOfDay)

        TimeZone ->
            return (ConT ''Time.TimeZone)

        App List dhallElementType -> do
            haskellElementType <- loop dhallElementType

            return (AppT (ConT ''[]) haskellElementType)

        App Optional dhallElementType -> do
            haskellElementType <- loop dhallElementType

            return (AppT (ConT ''Maybe) haskellElementType)

        App dhallAppType dhallElementType -> do
            haskellAppType <- loop dhallAppType
            haskellElementType <- loop dhallElementType

            return (AppT haskellAppType haskellElementType)

        Var v
            | Just (V param index) <- List.find (v ==) typeParams -> do
                let name = TH.mkName $ (Text.unpack param) ++ (show index)

                return (VarT name)

            | otherwise -> fail $ message v

        _   | Just haskellType <- List.find (predicate dhallType) haskellTypes -> do
                let name = TH.mkName (Text.unpack (typeName haskellType))

                return (ConT name)
            | otherwise -> fail $ message dhallType

-- | A deriving clause for `Generic`.
derivingGenericClause :: DerivClause
derivingGenericClause = DerivClause (Just StockStrategy) [ ConT ''Generic ]

-- | Generates a `FromDhall` instances.
fromDhallInstance
    :: TH.Name -- ^ The name of the type the instances is for
    -> Q Exp       -- ^ A TH splice generating some `Dhall.InterpretOptions`
    -> Q [Dec]
fromDhallInstance n interpretOptions = [d|
    instance FromDhall $(pure $ ConT n) where
        autoWith = Dhall.genericAutoWithInputNormalizer $(interpretOptions)
    |]

-- | Generates a `ToDhall` instances.
toDhallInstance
    :: TH.Name -- ^ The name of the type the instances is for
    -> Q Exp       -- ^ A TH splice generating some `Dhall.InterpretOptions`
    -> Q [Dec]
toDhallInstance n interpretOptions = [d|
    instance ToDhall $(pure $ ConT n) where
        injectWith = Dhall.genericToDhallWithInputNormalizer $(interpretOptions)
    |]

-- | Convert a Dhall type to the corresponding Haskell datatype declaration
toDeclaration
    :: (Eq a, Pretty a)
    => GenerateOptions
    -> [HaskellType (Expr s a)]
    -> HaskellType (Expr s a)
    -> Q [Dec]
toDeclaration generateOptions@GenerateOptions{..} haskellTypes typ =
    case typ of
        SingleConstructor{..} -> uncurry (fromSingle typeName constructorName) $ getTypeParams code
        MultipleConstructors{..} -> uncurry (fromMulti typeName) $ getTypeParams code
    where
        getTypeParams = first numberConsecutive .  getTypeParams_ []

        getTypeParams_ acc (Lam _ (FunctionBinding _ v _ _ _) rest) = getTypeParams_ (v:acc) rest
        getTypeParams_ acc rest = (acc, rest)

        derivingClauses = [ derivingGenericClause | generateFromDhallInstance || generateToDhallInstance ]

        interpretOptions = generateToInterpretOptions generateOptions typ

#if MIN_VERSION_template_haskell(2,21,0)
        toTypeVar (V n i) = TH.PlainTV (TH.mkName (Text.unpack n ++ show i)) TH.BndrInvis
#elif MIN_VERSION_template_haskell(2,17,0)
        toTypeVar (V n i) = TH.PlainTV (TH.mkName (Text.unpack n ++ show i)) ()
#else
        toTypeVar (V n i) = TH.PlainTV (TH.mkName (Text.unpack n ++ show i))
#endif

        toDataD typeName typeParams constructors = do
            let name = TH.mkName (Text.unpack typeName)

            let params = fmap toTypeVar typeParams

            fmap concat . sequence $
                [pure [DataD [] name params Nothing constructors derivingClauses]] <>
                [ fromDhallInstance name interpretOptions | generateFromDhallInstance ] <>
                [ toDhallInstance name interpretOptions | generateToDhallInstance ]

        fromSingle typeName constructorName typeParams dhallType = do
            constructor <- toConstructor typeParams generateOptions haskellTypes typeName (constructorName, Just dhallType)

            toDataD typeName typeParams [constructor]

        fromMulti typeName typeParams dhallType = case dhallType of
            Union kts -> do
                constructors <- traverse (toConstructor typeParams generateOptions haskellTypes typeName) (Dhall.Map.toList kts)

                toDataD typeName typeParams constructors

            _ -> fail $ message dhallType

        message dhallType = Pretty.renderString (Dhall.Pretty.layout $ document dhallType)

        document dhallType =
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
                , " " <> Dhall.Util.insert dhallType <> "\n"
                , "                                                                                \n"
                , "... which is not a union type."
                ]

-- | Number each variable, starting at 0
numberConsecutive :: [Text.Text] -> [Var]
numberConsecutive = snd . List.mapAccumR go Map.empty . reverse
  where
      go m k =
          let (i, m') = Map.updateLookupWithKey (\_ j -> Just $ j + 1) k m
          in maybe ((Map.insert k 0 m'), (V k 0)) (\i' -> (m', (V k i'))) i

-- | Convert a Dhall type to the corresponding Haskell constructor
toConstructor
    :: (Eq a, Pretty a)
    => [Var]
    -> GenerateOptions
    -> [HaskellType (Expr s a)]
    -> Text
    -- ^ typeName
    -> (Text, Maybe (Expr s a))
    -- ^ @(constructorName, fieldType)@
    -> Q Con
toConstructor typeParams GenerateOptions{..} haskellTypes outerTypeName (constructorName, maybeAlternativeType) = do
    let name = TH.mkName (Text.unpack $ constructorModifier constructorName)

    let strictness = if makeStrict then SourceStrict else NoSourceStrictness

    let bang = Bang NoSourceUnpackedness strictness

    case maybeAlternativeType of
        Just dhallType
            | let predicate haskellType =
                    Core.judgmentallyEqual (code haskellType) dhallType
                    && typeName haskellType /= outerTypeName
            , Just haskellType <- List.find predicate haskellTypes -> do
                let innerName =
                        TH.mkName (Text.unpack (typeName haskellType))

                return (NormalC name [ (bang, ConT innerName) ])

        Just (Record kts) -> do
            let process (key, dhallFieldType) = do
                    haskellFieldType <- toNestedHaskellType typeParams haskellTypes dhallFieldType

                    return (TH.mkName (Text.unpack $ fieldModifier key), bang, haskellFieldType)

            varBangTypes <- traverse process (Dhall.Map.toList $ Core.recordFieldValue <$> kts)

            return (RecC name varBangTypes)

        Just dhallAlternativeType -> do
            haskellAlternativeType <- toNestedHaskellType typeParams haskellTypes dhallAlternativeType

            return (NormalC name [ (bang, haskellAlternativeType) ])

        Nothing ->
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

-- | Used by `makeHaskellTypes` and `makeHaskellTypesWith` to specify how to
-- generate Haskell types.
data HaskellType code
    -- | Generate a Haskell type with more than one constructor from a Dhall
    -- union type.
    = MultipleConstructors
        { typeName :: Text
        -- ^ Name of the generated Haskell type
        , code :: code
        -- ^ Dhall code that evaluates to a union type
        }
    -- | Generate a Haskell type with one constructor from any Dhall type.
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

-- | This data type holds various options that let you control several aspects
-- how Haskell code is generated. In particular you can
--
--   * disable the generation of `FromDhall`/`ToDhall` instances.
--   * modify how a Dhall union field translates to a Haskell data constructor.
data GenerateOptions = GenerateOptions
    { constructorModifier :: Text -> Text
    -- ^ How to map a Dhall union field name to a Haskell constructor.
    -- Note: The `constructorName` of `SingleConstructor` will be passed to this function, too.
    , fieldModifier :: Text -> Text
    -- ^ How to map a Dhall record field names to a Haskell record field names.
    , generateFromDhallInstance :: Bool
    -- ^ Generate a `FromDhall` instance for the Haskell type
    , generateToDhallInstance :: Bool
    -- ^ Generate a `ToDhall` instance for the Haskell type
    , makeStrict :: Bool
    -- ^ Make all fields strict.
    }

-- | A default set of options used by `makeHaskellTypes`. That means:
--
--     * Constructors and fields are passed unmodified.
--     * Both `FromDhall` and `ToDhall` instances are generated.
--
--   Note: `From/ToDhall` should be `False` if importing higher-kinded types.
--   In these cases one should use a standalone declaration.
defaultGenerateOptions :: GenerateOptions
defaultGenerateOptions = GenerateOptions
    { constructorModifier = id
    , fieldModifier = id
    , generateFromDhallInstance = True
    , generateToDhallInstance = True
    , makeStrict = False
    }

-- | This function generates `Dhall.InterpretOptions` that can be used for the
--   marshalling of the Haskell type generated according to the `GenerateOptions`.
--   I.e. those `Dhall.InterpretOptions` reflect the mapping done by
--   `constructorModifier` and `fieldModifier` on the value level.
generateToInterpretOptions :: GenerateOptions -> HaskellType (Expr s a) -> Q Exp
generateToInterpretOptions GenerateOptions{..} haskellType = [| Dhall.InterpretOptions
    { Dhall.fieldModifier = \ $(pure nameP) ->
        $(toCases fieldModifier $ fields haskellType)
    , Dhall.constructorModifier = \ $(pure nameP) ->
        $(toCases constructorModifier $ constructors haskellType)
    , Dhall.singletonConstructors = Dhall.singletonConstructors Dhall.defaultInterpretOptions
    }|]
    where
        constructors :: HaskellType (Expr s a) -> [Text]
        constructors SingleConstructor{..} = [constructorName]
        constructors MultipleConstructors{..} | Union kts <- code = Dhall.Map.keys kts
        constructors _ = []

        fields :: HaskellType (Expr s a) -> [Text]
        fields SingleConstructor{..} | Record kts <- code = Dhall.Map.keys kts
        fields MultipleConstructors{..} | Union kts <- code = Set.toList $ mconcat
            [ Dhall.Map.keysSet kts'
            | (_, Just (Record kts')) <- Dhall.Map.toList kts
            ]
        fields _ = []

        toCases :: (Text -> Text) -> [Text] -> Q Exp
        toCases f xs = do
            err <- [| Core.internalError $ "Unmatched " <> Text.pack (show $(pure nameE)) |]
            pure $ CaseE nameE $ map mkMatch xs <> [Match WildP (NormalB err) []]
            where
                mkMatch n = Match (textToPat $ f n) (NormalB $ textToExp n) []

        nameE :: Exp
        nameE = TH.VarE $ TH.mkName "n"

        nameP :: Pat
        nameP = TH.VarP $ TH.mkName "n"

        textToExp :: Text -> Exp
        textToExp = TH.LitE . TH.StringL . Text.unpack

        textToPat :: Text -> Pat
        textToPat = TH.LitP . TH.StringL . Text.unpack

-- | Generate a Haskell datatype declaration with one constructor from a Dhall
-- type.
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
-- as `Eq`\/`Ord`\/`Show`), you can use the @StandaloneDeriving@ language
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
makeHaskellTypes = makeHaskellTypesWith defaultGenerateOptions

-- | Like `makeHaskellTypes`, but with the ability to customize the generated
-- Haskell code by passing `GenerateOptions`.
--
-- For instance, `makeHaskellTypes` is implemented using this function:
--
-- > makeHaskellTypes = makeHaskellTypesWith defaultGenerateOptions
makeHaskellTypesWith :: GenerateOptions -> [HaskellType Text] -> Q [Dec]
makeHaskellTypesWith generateOptions haskellTypes = do
    TH.runIO (GHC.IO.Encoding.setLocaleEncoding System.IO.utf8)

    haskellTypes' <- traverse (traverse (TH.runIO . Dhall.inputExpr)) haskellTypes

    concat <$> traverse (toDeclaration generateOptions haskellTypes') haskellTypes'
