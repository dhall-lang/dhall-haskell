{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Data.Map.Strict                       as Data.Map
import qualified Data.Text                             as Text
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as PrettyText
import qualified Dhall.Core                            as Dhall
import qualified Dhall.Format
import qualified Dhall.Pretty
import qualified Dhall.Util
import qualified Turtle

import           Data.Aeson                            (decodeFileStrict)
import           Data.Bifunctor                        (bimap)
import           Data.Foldable                         (for_)
import           Data.Text                             (Text, pack)
import qualified Options.Applicative
import           Control.Applicative.Combinators       (sepBy1, option)
import           Text.Megaparsec                       (some, parse, (<|>), errorBundlePretty)
import           Text.Megaparsec.Char                  (char, alphaNumChar)

import qualified Dhall.Kubernetes.Convert              as Convert
import           Dhall.Kubernetes.Data                 (patchCyclicImports)
import qualified Dhall.Parser
import           Dhall.Kubernetes.Types


-- | Top-level program options
data Options = Options
    { skipDuplicates :: Bool
    , prefixMap :: Data.Map.Map Prefix Dhall.Import
    , filename :: String
    }

-- | Write and format a Dhall expression to a file
writeDhall :: Turtle.FilePath -> Expr -> IO ()
writeDhall path expr = do
  echoStr $ "Writing file '" <> Turtle.encodeString path <> "'"
  Turtle.writeTextFile path $ pretty expr <> "\n"

  let characterSet = Dhall.Pretty.ASCII

  let censor = Dhall.Util.NoCensor

  let formatMode =
          Dhall.Format.Modify (Dhall.Util.InputFile (Turtle.encodeString path))

  let formatOptions = Dhall.Format.Format{..}

  Dhall.Format.format formatOptions

-- | Pretty print things
pretty :: Pretty.Pretty a => a -> Text
pretty = PrettyText.renderStrict
  . Pretty.layoutPretty Pretty.defaultLayoutOptions
  . Pretty.pretty

echo :: Turtle.MonadIO m => Text -> m ()
echo = Turtle.printf (Turtle.s Turtle.% "\n")

echoStr :: Turtle.MonadIO m => String -> m ()
echoStr = echo . Text.pack

errorOnDuplicateHandler :: DuplicateHandler
errorOnDuplicateHandler (kind, names) = error $ "Got more than one key for "++ show kind ++"! See:\n" <> show names

skipDuplicatesHandler :: DuplicateHandler
skipDuplicatesHandler = const Nothing

parseImport :: String -> Expr -> Dhall.Parser.Parser Dhall.Import
parseImport _ (Dhall.Note _ (Dhall.Embed l)) = pure l
parseImport prefix e = fail $ "Expected a Dhall import for " <> prefix <> " not:\n" <> show e

parsePrefixMap :: Options.Applicative.ReadM (Data.Map.Map Prefix Dhall.Import)
parsePrefixMap =
  Options.Applicative.eitherReader $ \s ->
    bimap errorBundlePretty Data.Map.fromList $ result (pack s)
  where
    parser = do
      prefix <- some (alphaNumChar <|> char '.')
      char '='
      e <- Dhall.Parser.expr
      imp <- parseImport prefix e
      return (pack prefix, imp)
    result = parse (Dhall.Parser.unParser parser `sepBy1` char ',') "MAPPING"

parseOptions :: Options.Applicative.Parser Options
parseOptions = Options <$> parseSkip <*> parsePrefixMap' <*> fileArg
  where
    parseSkip =
      Options.Applicative.switch
        (  Options.Applicative.long "skipDuplicates"
        <> Options.Applicative.help "Skip types with the same name when aggregating types"
        )
    parsePrefixMap' =
      option Data.Map.empty $ Options.Applicative.option parsePrefixMap
        (  Options.Applicative.long "prefixMap"
        <> Options.Applicative.help "Specify prefix mappings as 'prefix1=importBase1,prefix2=importBase2,...'"
        <> Options.Applicative.metavar "MAPPING"
        )
    fileArg = Options.Applicative.strArgument
            (  Options.Applicative.help "The swagger file to read"
            <> Options.Applicative.metavar "FILE"
            )

-- | `ParserInfo` for the `Options` type
parserInfoOptions :: Options.Applicative.ParserInfo Options
parserInfoOptions =
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.progDesc "Swagger to Dhall generator"
        <>  Options.Applicative.fullDesc
        )

main :: IO ()
main = do
  Options{..} <- Options.Applicative.execParser parserInfoOptions
  let duplicateHandler = if skipDuplicates then skipDuplicatesHandler else errorOnDuplicateHandler
  -- Get the Swagger spec
  Swagger{..} <- do
    swaggerFile <- decodeFileStrict filename
    case swaggerFile of
      Nothing -> error "Unable to decode the Swagger file"
      Just s  -> pure s

  -- Convert to Dhall types in a Map
  let types = Convert.toTypes prefixMap
        -- TODO: find a better way to deal with this cyclic import
         $ Data.Map.adjust patchCyclicImports
            (ModelName "io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1beta1.JSONSchemaProps")
            definitions

  -- Output to types
  Turtle.mktree "types"
  for_ (Data.Map.toList types) $ \(ModelName name, expr) -> do
    let path = "./types" Turtle.</> Turtle.fromText (name <> ".dhall")
    writeDhall path expr

  -- Convert from Dhall types to defaults
  let defaults = Data.Map.mapMaybeWithKey (Convert.toDefault prefixMap definitions types) types

  -- Output to defaults
  Turtle.mktree "defaults"
  for_ (Data.Map.toList defaults) $ \(ModelName name, expr) -> do
    let path = "./defaults" Turtle.</> Turtle.fromText (name <> ".dhall")
    writeDhall path expr

  let toSchema (ModelName key) _ _ =
        Dhall.RecordLit
          [ ("Type", Dhall.Embed (Convert.mkImport prefixMap ["types", ".."] (key <> ".dhall")))
          , ("default", Dhall.Embed (Convert.mkImport prefixMap ["defaults", ".."] (key <> ".dhall")))
          ]

  let schemas = Data.Map.intersectionWithKey toSchema types defaults

  -- Output schemas that combine both the types and defaults
  Turtle.mktree "schemas"
  for_ (Data.Map.toList schemas) $ \(ModelName name, expr) -> do
    let path = "./schemas" Turtle.</> Turtle.fromText (name <> ".dhall")
    writeDhall path expr

  -- Output the types record, the defaults record, and the giant union type
  let getImportsMap = Convert.getImportsMap prefixMap duplicateHandler objectNames
      objectNames = Data.Map.keys types
      typesMap = getImportsMap "types" $ Data.Map.keys types
      defaultsMap = getImportsMap "defaults" $ Data.Map.keys defaults
      schemasMap = getImportsMap "schemas" $ Data.Map.keys schemas

      typesRecordPath = "./types.dhall"
      typesUnionPath = "./typesUnion.dhall"
      defaultsRecordPath = "./defaults.dhall"
      schemasRecordPath = "./schemas.dhall"

  writeDhall typesUnionPath (Dhall.Union $ fmap Just typesMap)
  writeDhall typesRecordPath (Dhall.RecordLit typesMap)
  writeDhall defaultsRecordPath (Dhall.RecordLit defaultsMap)
  writeDhall schemasRecordPath (Dhall.RecordLit schemasMap)
