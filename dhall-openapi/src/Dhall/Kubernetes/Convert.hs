{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

module Dhall.Kubernetes.Convert
  ( toTypes
  , toDefault
  , getImportsMap
  , mkImport
  , toDefinition
  ) where

import Control.Applicative (empty)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Bifunctor (first, second)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Dhall.Kubernetes.Types
import GHC.Generics (Generic, Rep)

import qualified Data.Char         as Char
import qualified Data.List         as List
import qualified Data.Map.Strict   as Data.Map
import qualified Data.Set          as Set
import qualified Data.Sort         as Sort
import qualified Data.Text         as Text
import qualified Dhall.Core        as Dhall
import qualified Dhall.Map
import qualified Dhall.Optics

-- | Get all the required fields for a model
--   See https://kubernetes.io/docs/concepts/overview/working-with-objects/kubernetes-objects/#required-fields
--   TLDR: because k8s API allows PUTS etc with partial data,
--   it's not clear from the data types OR the API which
--   fields are required for A POST...
requiredFields :: Maybe ModelName -> Maybe (Set FieldName) -> Set FieldName
requiredFields maybeName required
  = Set.difference
      (List.foldr Set.union (fromMaybe Set.empty required) [alwaysRequired, toAdd])
      toRemove
  where
    alwaysRequired = Set.fromList $ FieldName <$> [ "apiVersion", "kind", "metadata"]
    toAdd = fromMaybe Set.empty $ do
      name <- maybeName
      Data.Map.lookup name requiredConstraints
    toRemove = fromMaybe Set.empty $ do
      name <- maybeName
      Data.Map.lookup name notRequiredConstraints

    -- | Some models require keys that are not in the required set,
    --   but are in the docs or just work
    requiredConstraints = Data.Map.fromList [ ]

    -- | Some models should not require some keys, and this is not
    --   in the Swagger spec but just in the docs
    notRequiredConstraints = Data.Map.fromList
      [ ( ModelName "io.k8s.api.core.v1.ObjectFieldSelector"
        , Set.fromList [ FieldName "apiVersion" ]
        )
      , ( ModelName "io.k8s.apimachinery.pkg.apis.meta.v1.StatusDetails"
        , Set.fromList [ FieldName "kind" ]
        )
      , ( ModelName "io.k8s.api.core.v1.PersistentVolumeClaim"
        , Set.fromList [ FieldName "apiVersion", FieldName "kind" ]
        )
      ]


-- | Get a filename from a Swagger ref
pathFromRef :: Ref -> Text
pathFromRef (Ref r) = (Text.split (== '/') r) List.!! 2

-- | Build an import from path components (note: they need to be in reverse order)
--   and a filename
mkImport :: Data.Map.Map Prefix Dhall.Import -> [Text] -> Text -> Dhall.Import
mkImport prefixMap components file =
  case Data.Map.toList filteredPrefixMap of
    []    -> localImport
    xs    -> (snd . head $ Sort.sortOn (Text.length . fst) xs) <> localImport
  where
    localImport = Dhall.Import{..}
    importMode = Dhall.Code
    importHashed = Dhall.ImportHashed{..}
    hash = Nothing
    importType = Dhall.Local Dhall.Here Dhall.File{..}
    directory = Dhall.Directory{..}
    filteredPrefixMap = Data.Map.filterWithKey (\k _ -> Text.isPrefixOf k file) prefixMap

-- | Get the namespaced object name if the Import points to it
namespacedObjectFromImport :: Dhall.Import -> Maybe Text
namespacedObjectFromImport Dhall.Import
  { importHashed = Dhall.ImportHashed
    { importType = Dhall.Local Dhall.Here Dhall.File
      { file = f , .. }
    , .. }
  , .. } = Just $ Text.replace ".dhall" "" f
namespacedObjectFromImport _ = Nothing

-- | Get a Dhall Text literal from a lone string
toTextLit :: Text -> Expr
toTextLit str = Dhall.TextLit (Dhall.Chunks [] str)


{-| Converts all the Swagger definitions to Dhall Types

    Note: we cannot do 1-to-1 conversion and we need the whole Map because
    many types reference other types so we need to access them to decide things
    like "should this key be optional"
-}
toTypes :: Data.Map.Map Prefix Dhall.Import -> Data.Map.Map ModelName Definition -> Data.Map.Map ModelName Expr
toTypes prefixMap definitions = memo
  where
    memo = Data.Map.mapWithKey (\k -> convertToType (Just k)) definitions

    kvList = Dhall.App Dhall.List $ Dhall.Record $ Dhall.Map.fromList
      [ ("mapKey", Dhall.makeRecordField Dhall.Text), ("mapValue", Dhall.makeRecordField Dhall.Text) ]
    intOrString = Dhall.Union $ Dhall.Map.fromList $ fmap (second Just)
      [ ("Int", Dhall.Natural), ("String", Dhall.Text) ]

    shouldBeRequired :: Maybe ModelName -> FieldName -> Bool
    shouldBeRequired maybeParent field = Set.member field requiredNames
      where
        requiredNames = requiredFields maybeParent $ do
          name <- maybeParent
          Definition{..} <- Data.Map.lookup name definitions
          required

    -- | Convert a single Definition to a Dhall Type
    --   Note: we have the ModelName only if this is a top-level import
    convertToType :: Maybe ModelName -> Definition -> Expr
    convertToType maybeModelName Definition{..} = case (ref, typ, properties) of
      -- If we point to a ref we just reference it via Import
      (Just r, _, _) -> Dhall.Embed $ mkImport prefixMap [] (pathFromRef r <> ".dhall")
      -- Otherwise - if we have 'properties' - it's an object
      (_, _, Just props) ->
        let (required', optional')
              = Data.Map.partitionWithKey
                (\k _ -> shouldBeRequired maybeModelName (FieldName (unModelName k)))
              -- TODO: labelize
              $ Data.Map.map (convertToType Nothing) props

            allFields
              = Data.Map.toList required'
              <> fmap (second $ Dhall.App Dhall.Optional) (Data.Map.toList optional')

            adaptRecordList = Dhall.Map.mapMaybe (Just . Dhall.makeRecordField)

        in Dhall.Record $ adaptRecordList $ Dhall.Map.fromList $ fmap (first $ unModelName) allFields
      -- Otherwise - if we have a 'type' - it's a basic type
      (_, Just basic, _) -> case basic of
        "object"  -> kvList
        "array"   | Just item <- items -> Dhall.App Dhall.List (convertToType Nothing item)
        "string"  | format == Just "int-or-string" -> intOrString
        "string"  -> Dhall.Text
        "boolean" -> Dhall.Bool
        "integer" -> Dhall.Natural
        "number"  -> Dhall.Double
        other     -> error $ "Found missing Swagger type: " <> Text.unpack other
      -- There are empty schemas that only have a description, so we return empty record
      _ -> Dhall.Record mempty


-- | Convert a Dhall Type to its default value
toDefault
  :: Data.Map.Map Prefix Dhall.Import  -- ^ Mapping of prefixes to import roots
  -> Data.Map.Map ModelName Definition -- ^ All the Swagger definitions
  -> ModelName                         -- ^ The name of the object we're converting
  -> Expr                              -- ^ The Dhall type of the object
  -> Maybe Expr
toDefault prefixMap definitions modelName = go
  where
    go = \case
      -- If we have an import, we also import in the default
      e@(Dhall.Embed _) -> Just e
      -- For a sum type, there is no obvious default value
      Dhall.Union _ -> Nothing
      -- Dynamic records (i.e. List { mapKey : Text, mapValue : Text }) also
      -- don't have default
      Dhall.App Dhall.List _ -> Nothing
      -- Simple types should not have a default
      Dhall.Text -> Nothing
      Dhall.Natural -> Nothing
      -- But most of the times we are dealing with a record.
      -- Here we transform the record type in a value, transforming the keys in
      -- this way:
      --
      --   * take the BaseData from definition and populate it
      --   * skip other required fields, except if they are records
      --   * set the optional fields to None and the lists to empty
      Dhall.Record kvsf ->
        let getBaseData :: Maybe Definition -> Dhall.Map.Map Text Expr
            getBaseData (Just Definition{ baseData = Just BaseData{..} }) =
                Dhall.Map.fromList
                    [ ("apiVersion", toTextLit apiVersion)
                    , ("kind"      , toTextLit kind      )
                    ]
            getBaseData _ = mempty

            baseData = getBaseData $ Data.Map.lookup modelName definitions

            -- | Given a Dhall type from a record field, figure out if and what
            --   default value it should have
            valueForField :: Expr -> Maybe Expr
            valueForField = \case
                Dhall.App Dhall.Optional _T -> do
                    let expression = Dhall.App Dhall.None _T

                    let adjustedExpression =
                            Dhall.Optics.transformOf
                              Dhall.subExpressions
                              adjustImport
                              expression

                    return adjustedExpression
                _ -> do
                    empty

            kvs = Dhall.Map.mapMaybe (Just . Dhall.recordFieldValue) kvsf

            adaptRecordMap = Dhall.Map.mapMaybe (Just . Dhall.makeRecordField)

            -- The main reason for adding this special case is so that the
            -- `apiVersion` and `kind` fields default to `Some …` instead of
            -- `None …`.
            combine l (Dhall.App Dhall.None _T) = Dhall.Some l
            combine _  r                        = r

        in  Just $ Dhall.RecordLit $ adaptRecordMap $ Dhall.Map.unionWith combine baseData (Dhall.Map.mapMaybe valueForField kvs)

      -- We error out here because wildcards are bad, and we should know if
      -- we get something unexpected
      e -> error $ show modelName <> "\n\n" <> show e

    -- | The imports that we get from the types are referring to the local folder,
    --   but if we want to refer them from the defaults we need to adjust the path
    adjustImport :: Expr -> Expr
    adjustImport (Dhall.Embed imp) | Just file <- namespacedObjectFromImport imp
      = Dhall.Embed $ mkImport prefixMap ["types", ".."] (file <> ".dhall")
    adjustImport other = other


-- | Get a Dhall.Map filled with imports, for creating giant Records or Unions of types or defaults
getImportsMap
  :: Data.Map.Map Prefix Dhall.Import -- ^ Mapping of prefixes to import roots
  -> DuplicateHandler                 -- ^ Duplicate name handler
  -> [ModelName]                      -- ^ A list of all the object names
  -> Text                             -- ^ The folder we should get imports from
  -> [ModelName]                      -- ^ List of the object names we want to include in the Map
  -> Dhall.Map.Map Text Expr
getImportsMap prefixMap duplicateNameHandler objectNames folder toInclude
  = Dhall.Map.fromList
  $ Data.Map.elems
  -- This intersection is here to "pick" common elements between "all the objects"
  -- and "objects we want to include", already associating keys to their import
  $ Data.Map.intersectionWithKey
      (\(ModelName name) key _ -> (key, Dhall.Embed $ mkImport prefixMap [folder] (name <> ".dhall")))
      namespacedToSimple
      (Data.Map.fromList $ fmap (,()) toInclude)
  where
    -- | A map from namespaced names to simple ones (i.e. without the namespace)
    namespacedToSimple
      = Data.Map.fromList $ mapMaybe selectObject $ Data.Map.toList $ groupByObjectName objectNames

    -- | Given a list of fully namespaced bjects, it will group them by the
    --   object name
    groupByObjectName :: [ModelName] -> Data.Map.Map Text [ModelName]
    groupByObjectName modelNames = Data.Map.unionsWith (<>)
      $ (\name -> Data.Map.singleton (getKind name) [name])
      <$> modelNames
      where
        getKind (ModelName name) =
          let elems = Text.split (== '.') name
          in elems List.!! (length elems - 1)

    -- | There will be more than one namespaced object for a single object name
    --   (because different API versions, and objects move around packages but k8s
    --   cannot break compatibility so we have all of them), so we have to select one
    --   (and we error out if it's not so after the filtering)
    selectObject :: (Text, [ModelName]) -> Maybe (ModelName, Text)
    selectObject (kind, namespacedNames) = fmap (,kind) namespaced
      where
        filterFn (ModelName name) = not $ or
          -- The reason why we filter these two prefixes is that they are "internal"
          -- objects. I.e. they do not appear referenced in other objects, but are
          -- just in the Go source. E.g. see https://godoc.org/k8s.io/kubernetes/pkg/apis/core
          [ Text.isPrefixOf "io.k8s.kubernetes.pkg.api." name
          , Text.isPrefixOf "io.k8s.kubernetes.pkg.apis." name
          -- We keep a list of "old" objects that should not be preferred/picked
          ]

        namespaced = case filter filterFn namespacedNames of
          [name] -> Just name
          []     -> Nothing
          names  -> duplicateNameHandler (kind, names)

stripPrefix :: (Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
stripPrefix n = genericParseJSON options
  where
    options = defaultOptions { fieldLabelModifier }

    fieldLabelModifier string = case drop n string of
        s : tring -> Char.toLower s : tring
        []        -> []

data V1CustomResourceDefinition =
    V1CustomResourceDefinition
        { v1CustomResourceDefinitionSpec :: V1CustomResourceDefinitionSpec
        }
    deriving (Generic)


instance FromJSON V1CustomResourceDefinition where
    parseJSON = stripPrefix 26

data V1CustomResourceDefinitionSpec =
    V1CustomResourceDefinitionSpec
        { v1CustomResourceDefinitionSpecGroup :: Text
        , v1CustomResourceDefinitionSpecNames :: V1CustomResourceDefinitionNames
        , v1CustomResourceDefinitionSpecVersions :: Maybe [V1CustomResourceDefinitionVersion]
        } deriving (Generic)

instance FromJSON V1CustomResourceDefinitionSpec where
    parseJSON = stripPrefix 30

data V1CustomResourceDefinitionNames =
    V1CustomResourceDefinitionNames
        { v1CustomResourceDefinitionNamesKind :: Text
        } deriving (Generic)

instance FromJSON V1CustomResourceDefinitionNames where
    parseJSON = stripPrefix 31

data V1CustomResourceValidation =
    V1CustomResourceValidation
        { v1CustomResourceValidationOpenAPIV3Schema :: Maybe V1JSONSchemaProps
        } deriving (Generic)

instance FromJSON V1CustomResourceValidation where
    parseJSON = stripPrefix 26

data V1CustomResourceDefinitionVersion =
    V1CustomResourceDefinitionVersion
        { v1CustomResourceDefinitionVersionName :: Text
        , v1CustomResourceDefinitionVersionSchema :: Maybe V1CustomResourceValidation
        } deriving (Generic)

instance FromJSON V1CustomResourceDefinitionVersion where
    parseJSON = stripPrefix 33

data V1JSONSchemaProps =
    V1JSONSchemaProps
        { v1JSONSchemaPropsRef :: Maybe Text
        , v1JSONSchemaPropsDescription :: Maybe Text
        , v1JSONSchemaPropsFormat :: Maybe Text
        , v1JSONSchemaPropsItems :: Maybe Value
        , v1JSONSchemaPropsProperties :: Maybe (Data.Map.Map String V1JSONSchemaProps)
        , v1JSONSchemaPropsRequired :: Maybe [Text]
        , v1JSONSchemaPropsType :: Maybe Text
        } deriving (Generic)

instance FromJSON V1JSONSchemaProps where
    parseJSON = stripPrefix 17

mkV1JSONSchemaProps :: V1JSONSchemaProps
mkV1JSONSchemaProps =
    V1JSONSchemaProps
        { v1JSONSchemaPropsRef = Nothing
        , v1JSONSchemaPropsDescription = Nothing
        , v1JSONSchemaPropsFormat = Nothing
        , v1JSONSchemaPropsItems = Nothing
        , v1JSONSchemaPropsProperties = Nothing
        , v1JSONSchemaPropsRequired = Nothing
        , v1JSONSchemaPropsType = Nothing
        }

orDie :: Maybe a -> e -> Either e a
Just r  `orDie` _ = Right r
Nothing `orDie` l = Left  l

toDefinition
    :: V1CustomResourceDefinition -> Either Text (ModelName, Definition)
toDefinition crd = fmap (\d -> (modelName, d)) definition
  where
    V1CustomResourceDefinition{..} = crd

    V1CustomResourceDefinitionSpec{..} = v1CustomResourceDefinitionSpec

    V1CustomResourceDefinitionNames{..} = v1CustomResourceDefinitionSpecNames

    modelName =
        ModelName (v1CustomResourceDefinitionSpecGroup <> "." <> v1CustomResourceDefinitionNamesKind)

    definition = do
      versions <- v1CustomResourceDefinitionSpecVersions
          `orDie` "The CustomResourceDefinitionSpec is missing the versions field"

      V1CustomResourceDefinitionVersion{..} <- case versions of
          [ version ] ->
              return version
          _ ->
              Left "This tool does not yet support more than one version for the versions field of the CustomResourceDefinitionSpec"

      V1CustomResourceValidation{..} <- v1CustomResourceDefinitionVersionSchema
          `orDie` "The CustomResourceDefinitionSpec is missing the schema field"

      openApiv3Schema <- v1CustomResourceValidationOpenAPIV3Schema
          `orDie` "The CustomResourceValidation is missing the openApiv3Schema field"

      let baseData = BaseData {
            kind = v1CustomResourceDefinitionNamesKind,

            apiVersion = v1CustomResourceDefinitionVersionName
          }

      let completeSchemaProperties =
            fmap
            (Data.Map.union
              (
                Data.Map.fromList [
                  ("apiVersion", (mkV1JSONSchemaProps {v1JSONSchemaPropsType = Just "string"}))
                , ("kind", (mkV1JSONSchemaProps {v1JSONSchemaPropsType = Just "string"} ))
                , ("metadata", mkV1JSONSchemaProps {v1JSONSchemaPropsType = Just "object", v1JSONSchemaPropsRef = Just "#/definitions/io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta"} )
                ]
              ))
            (v1JSONSchemaPropsProperties openApiv3Schema)

      let completeSchema = openApiv3Schema { v1JSONSchemaPropsProperties = completeSchemaProperties}

      pure $ propsToDefinition completeSchema (Just baseData)

    propsToDefinition :: V1JSONSchemaProps -> Maybe BaseData -> Definition
    propsToDefinition V1JSONSchemaProps{..} basedata =
      Definition
        { typ         = v1JSONSchemaPropsType
        , ref         = Ref <$> v1JSONSchemaPropsRef
        , format      = v1JSONSchemaPropsFormat
        , description = v1JSONSchemaPropsDescription
        , items       = v1JSONSchemaPropsItems >>= parseMaybe parseJSON
        , properties  = fmap toProperties v1JSONSchemaPropsProperties
        , required    = fmap (Set.fromList . fmap FieldName) v1JSONSchemaPropsRequired
        , baseData    = basedata
        }

    toProperties :: Data.Map.Map String V1JSONSchemaProps -> Data.Map.Map ModelName Definition
    toProperties props =
      (Data.Map.fromList . fmap (\(k, p) -> ((ModelName . Text.pack) k, propsToDefinition p Nothing)) . Data.Map.toList) props
