module Dhall.Kubernetes.Convert
  ( toTypes
  , toDefault
  , getImportsMap
  ) where

import qualified Data.List              as List
import qualified Data.Map.Strict        as Data.Map
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import qualified Dhall.Core             as Dhall
import qualified Dhall.Map

import           Data.Bifunctor         (first, second)
import           Data.Maybe             (fromMaybe)
import           Data.Set               (Set)
import           Data.Text              (Text)

import           Dhall.Kubernetes.Data  (excludedModels)
import           Dhall.Kubernetes.Types


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
    requiredConstraints = Data.Map.fromList
      [ ( ModelName "io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta"
        , Set.fromList [FieldName "name"])
      ]

    -- | Some models should not require some keys, and this is not
    --   in the Swagger spec but just in the docs
    notRequiredConstraints = Data.Map.fromList
      [ ( ModelName "io.k8s.api.core.v1.ObjectFieldSelector"
        , Set.fromList [FieldName "apiVersion"])
      , ( ModelName "io.k8s.apimachinery.pkg.apis.meta.v1.StatusDetails"
        , Set.fromList [FieldName "kind"])
      ]


-- | Get a filename from a Swagger ref
pathFromRef :: Ref -> Text
pathFromRef (Ref r) = (Text.split (== '/') r) List.!! 2

-- | Build an import from path components (note: they need to be in reverse order)
--   and a filename
mkImport :: [Text] -> Text -> Dhall.Import
mkImport components file = Dhall.Import{..}
  where
    importMode = Dhall.Code
    importHashed = Dhall.ImportHashed{..}
    hash = Nothing
    importType = Dhall.Local Dhall.Here Dhall.File{..}
    directory = Dhall.Directory{..}

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


-- | Converts all the Swagger definitions to Dhall Types
--   Note: we cannot do 1-to-1 conversion and we need the whole Map because
--   many types reference other types so we need to access them to decide things
--   like "should this key be optional"
toTypes :: Data.Map.Map ModelName Definition -> Data.Map.Map ModelName Expr
toTypes definitions = memo
  where
    memo = Data.Map.mapWithKey (\k -> convertToType (Just k)) definitions

    kvList = Dhall.App Dhall.List $ Dhall.Record $ Dhall.Map.fromList
      [ ("mapKey", Dhall.Text), ("mapValue", Dhall.Text) ]
    intOrString = Dhall.Union $ Dhall.Map.fromList $ fmap (second Just)
      [ ("Int", Dhall.Natural), ("String", Dhall.Text) ]

    shouldBeRequired :: Maybe ModelName -> (Maybe FieldName, Expr) -> Bool
    shouldBeRequired maybeParent (maybeField, expr) = or
      -- | A field should not be optional if:
      -- * the field name is in the 'required' list
      [ case maybeField of
          Just field -> Set.member field requiredNames
          _ -> False
      -- * the field value is somewhat a container, and "transitively emptiable"
      --   (i.e. it could be an empty container, allowing dhall-to-yaml to throw
      --   away the empty shell if so)
      , case expr of
          -- So if it's a record we recursively check that all its fields can be emptiable
          (Dhall.Record kvs) -> not $ List.foldr (\a b -> or [a, b]) False
            $ shouldBeRequired maybeParent
            <$> (first (Just . FieldName))
            <$> Dhall.Map.toList kvs
          -- A list can indeed be empty
          (Dhall.App Dhall.List _) -> True
          -- An import requires us to recur on the toplevel Map of objects,
          -- to check if the object we're dealing with is itself an emptiable container
          (Dhall.Embed imp) ->
            let maybeModelName = fmap ModelName (namespacedObjectFromImport imp)
            in case maybeModelName >>= (\n -> Data.Map.lookup n memo) of
                 Just e -> shouldBeRequired maybeModelName (Nothing, e)
                 _      -> False
          _ -> False
      ]
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
      (Just r, _, _) -> Dhall.Embed $ mkImport [] $ (pathFromRef r <> ".dhall")
      -- Otherwise - if we have 'properties' - it's an object
      (_, _, Just props) ->
        let (required', optional')
              = Data.Map.partitionWithKey
                (\k v -> shouldBeRequired maybeModelName (Just $ FieldName $ unModelName k, v))
              -- TODO: labelize
              $ Data.Map.mapWithKey (\_k -> convertToType Nothing) props

            allFields
              = Data.Map.toList required'
              <> fmap (second $ Dhall.App Dhall.Optional) (Data.Map.toList optional')

        in Dhall.Record $ Dhall.Map.fromList $ fmap (first $ unModelName) allFields
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
  :: Data.Map.Map ModelName Definition -- ^ All the Swagger definitions
  -> Data.Map.Map ModelName Expr       -- ^ All the Dhall types generated from them
  -> ModelName                         -- ^ The name of the object we're converting
  -> Expr                              -- ^ The Dhall type of the object
  -> Maybe Expr
toDefault definitions types modelName = go
  where
    go = \case
      -- If we have an import, we also import in the default
      e@(Dhall.Embed _) -> Just e
      -- If it's a sum type, we have to exclude it as we cannot mix types and values
      -- in records (we need this to have the big "defaults" record)
      (Dhall.Union _) -> Nothing
      -- Simple types should not have a default
      (Dhall.Text) -> Nothing
      -- Set lists to empty
      (Dhall.App Dhall.List typ) -> Just $ Dhall.ListLit (Just $ adjustImport typ) mempty
      -- But most of the times we are dealing with a record.
      -- Here we transform the record type in a value, transforming the keys in this way:
      -- * take the BaseData from definition and populate it
      -- * skip other required fields, except if they are records
      -- * set the optional fields to None and the lists to empty
      (Dhall.Record kvs) ->
        let getBaseData :: Maybe Definition -> Dhall.Map.Map Text Expr
            getBaseData (Just Definition { baseData = Just BaseData{..} })
              = Dhall.Map.fromList [ ("apiVersion", toTextLit apiVersion)
                                   , ("kind", toTextLit kind)]
            getBaseData _ = mempty

            baseData = getBaseData $ Data.Map.lookup modelName definitions

            -- | Given a Dhall type from a record field, figure out if and what default
            --   value it should have
            valueForField :: Expr -> Maybe Expr
            valueForField = \case
              (Dhall.App Dhall.Optional typ) -> Just $ Dhall.App Dhall.None (adjustImport typ)
              (Dhall.App Dhall.List typ) -> Just $ Dhall.ListLit (Just $ adjustImport typ) mempty
              -- Imports can stay only if they refer to Records (which are "transitively emptiable")
              -- otherwise they have to go
              embed@(Dhall.Embed imp) -> do
                name <- namespacedObjectFromImport imp
                expr <- Data.Map.lookup (ModelName name) types
                case expr of
                  (Dhall.Record _) -> Just embed
                  _                -> Nothing
              _ -> Nothing

        in Just $ Dhall.RecordLit $ Dhall.Map.union baseData $ Dhall.Map.mapMaybe valueForField kvs

      -- We error out here because wildcards are bad, and we should know if
      -- we get something unexpected
      _ -> error $ show modelName

    -- | The imports that we get from the types are referring to the local folder,
    --   but if we want to refer them from the defaults we need to adjust the path
    adjustImport :: Expr -> Expr
    adjustImport (Dhall.Embed imp) | Just file <- namespacedObjectFromImport imp
      = Dhall.Embed $ mkImport ["types", ".."] (file <> ".dhall")
    adjustImport other = other


-- | Get a Dhall.Map filled with imports, for creating giant Records or Unions of types or defaults
getImportsMap
  :: [ModelName]             -- ^ A list of all the object names
  -> Text                    -- ^ The folder we should get imports from
  -> [ModelName]             -- ^ List of the object names we want to include in the Map
  -> Dhall.Map.Map Text Expr
getImportsMap objectNames folder toInclude
  = Dhall.Map.fromList
  $ Data.Map.elems
  -- This intersection is here to "pick" common elements between "all the objects"
  -- and "objects we want to include", already associating keys to their import
  $ Data.Map.intersectionWithKey
      (\(ModelName name) key _ -> (key, Dhall.Embed $ mkImport [folder] (name <> ".dhall")))
      namespacedToSimple
      (Data.Map.fromList $ fmap (,()) toInclude)
  where
    -- | A map from namespaced names to simple ones (i.e. without the namespace)
    namespacedToSimple
      = Data.Map.fromList $ fmap selectObject $ Data.Map.toList $ groupByObjectName objectNames

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
    selectObject :: (Text, [ModelName]) -> (ModelName, Text)
    selectObject (kind, namespacedNames) = (namespaced, kind)
      where
        filterFn modelName@(ModelName name) = not $ or
          -- The reason why we filter these two prefixes is that they are "internal"
          -- objects. I.e. they do not appear referenced in other objects, but are
          -- just in the Go source. E.g. see https://godoc.org/k8s.io/kubernetes/pkg/apis/core
          [ Text.isPrefixOf "io.k8s.kubernetes.pkg.api." name
          , Text.isPrefixOf "io.k8s.kubernetes.pkg.apis." name
          -- We keep a list of "old" objects that should not be preferred/picked
          , Set.member modelName excludedModels
          ]

        namespaced = case filter filterFn namespacedNames of
          [name] -> name
          wrong  -> error $ "Got more than one key for "++ show kind ++"! See:\n" <> show wrong
