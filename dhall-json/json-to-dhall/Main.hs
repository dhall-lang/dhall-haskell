{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| The tool for converting JSON data to Dhall given a Dhall /type/ expression necessary to make the translation unambiguous.

    Reasonable requirements to the conversion tool are:

    1. The Dhall type expression @/t/@ passed as an argument to @json-to-dhall@ should be a valid type of the resulting Dhall expression
    2. A JSON data produced by the corresponding @dhall-to-json@ from the Dhall expression of type @/t/@ should (under reasonable assumptions) reproduce the original Dhall expression using @json-to-dhall@ with type argument @/t/@

    Only a subset of Dhall types consisting of all the primitive types as well as @Optional@, @Union@ and @Record@ constructs, is used for reading JSON data:

    * @Bool@s
    * @Natural@s
    * @Integer@s
    * @Double@s
    * @Text@s
    * @List@s
    * @Optional@ values
    * unions
    * records

== Primitive types

    JSON @Bool@s translate to Dhall bools:

> $ json-to-dhall Bool <<< 'true'
> True
> $ json-to-dhall Bool <<< 'false'
> False

    JSON numbers translate to Dhall numbers:

> $ json-to-dhall Integer <<< 2
> +2
> $ json-to-dhall Natural <<< 2
> 2
> $ json-to-dhall Double <<< -2.345
> -2.345

    Dhall @Text@ corresponds to JSON text:

> $ json-to-dhall Text <<< '"foo bar"'
> "foo bar"


== Lists and records

    Dhall @List@s correspond to JSON lists:

> $ json-to-dhall 'List Integer' <<< '[1, 2, 3]'
> [ +1, +2, +3 ]


    Dhall __records__ correspond to JSON records:

> $ json-to-dhall '{foo : List Integer}' <<< '{"foo": [1, 2, 3]}'
> { foo = [ +1, +2, +3 ] }


    Note, that by default, only the fields required by the Dhall type argument are parsed (as you commonly will not need all the data), the remaining ones being ignored:

> $ json-to-dhall '{foo : List Integer}' <<< '{"foo": [1, 2, 3], "bar" : "asdf"}'
> { foo = [ +1, +2, +3 ] }


    If you do need to make sure that Dhall fully reflects JSON record data comprehensively, @--records-strict@ flag should be used:

> $ json-to-dhall --records-strict '{foo : List Integer}' <<< '{"foo": [1, 2, 3], "bar" : "asdf"}'
> Error: Key(s) @bar@ present in the JSON object but not in the corresponding Dhall record. This is not allowed in presence of --records-strict:


    By default, JSON key-value arrays will be converted to Dhall records:

> $ json-to-dhall '{ a : Integer, b : Text }' <<< '[{"key":"a", "value":1}, {"key":"b", "value":"asdf"}]'
> { a = +1, b = "asdf" }


    Attempting to do the same with @--no-keyval-arrays@ on will result in error:

> $ json-to-dhall --no-keyval-arrays '{ a : Integer, b : Text }' <<< '[{"key":"a", "value":1}, {"key":"b", "value":"asdf"}]'
> Error: JSON (key-value) arrays cannot be converted to Dhall records under --no-keyval-arrays flag:

    Conversion of the homogeneous JSON maps to the corresponding Dhall association lists by default:

> $ json-to-dhall 'List { mapKey : Text, mapValue : Text }' <<< '{"foo": "bar"}'
> [ { mapKey = "foo", mapValue = "bar" } ]

    Flag @--no-keyval-maps@ switches off this mechanism (if one would ever need it):

> $ json-to-dhall --no-keyval-maps 'List { mapKey : Text, mapValue : Text }' <<< '{"foo": "bar"}'
> Error: Homogeneous JSON map objects cannot be converted to Dhall association lists under --no-keyval-arrays flag


== Optional values and unions

    Dhall @Optional@ Dhall type allows null or missing JSON values:

> $ json-to-dhall "Optional Integer" <<< '1'
> Some +1

> $ json-to-dhall "Optional Integer" <<< null
> None Integer

> $ json-to-dhall '{ a : Integer, b : Optional Text }' <<< '{ "a": 1 }'
{ a = +1, b = None Text }



    For Dhall __union types__ the correct value will be based on matching the type of JSON expression:

> $ json-to-dhall 'List < Left : Text | Right : Integer >' <<< '[1, "bar"]'
> [ < Left : Text | Right : Integer >.Right +1
  , < Left : Text | Right : Integer >.Left "bar"
  ]

> $ json-to-dhall '{foo : < Left : Text | Right : Integer >}' <<< '{ "foo": "bar" }'
> { foo = < Left : Text | Right : Integer >.Left "bar" }

    In presence of multiple potential matches, the first will be selected by default:

> $ json-to-dhall '{foo : < Left : Text | Middle : Text | Right : Integer >}' <<< '{ "foo": "bar"}'
> { foo = < Left : Text | Middle : Text | Right : Integer >.Left "bar" }

    This will result in error if @--unions-strict@ flag is used, with the list of alternative matches being reported (as a Dhall list)

> $ json-to-dhall --unions-strict '{foo : < Left : Text | Middle : Text | Right : Integer >}' <<< '{ "foo": "bar"}'
> Error: More than one union component type matches JSON value
> ...
> Possible matches:
< Left : Text | Middle : Text | Right : Integer >.Left "bar"
> --------
< Left : Text | Middle : Text | Right : Integer >.Middle "bar"
-}

module Main where

import           Control.Applicative ((<|>))
import qualified Control.Exception
import           Control.Exception (SomeException, Exception, throwIO)
import           Control.Monad.Catch (throwM, MonadCatch)
import           Control.Monad (when)
import qualified Data.Aeson as A
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Either (rights)
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import           Data.List ((\\))
import           Data.Monoid ((<>))
import           Data.Scientific (floatingOrInteger, toRealFloat)
import qualified Data.Sequence as Seq
import qualified Data.String
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           Data.Text (Text)
import           Data.Version (showVersion)
import qualified GHC.IO.Encoding
import qualified Options.Applicative as O
import           Options.Applicative (Parser, ParserInfo)
import qualified System.Exit
import qualified System.IO

import qualified Dhall
import qualified Dhall.Core as D
import           Dhall.Core (Expr(App), Chunks(..))
import qualified Dhall.Import
import qualified Dhall.Map as Map
import qualified Dhall.Parser
import           Dhall.Parser (Src)
import qualified Dhall.TypeCheck as D
import           Dhall.TypeCheck (X)

import qualified Paths_dhall_json as Meta

-- ---------------
-- Command options
-- ---------------

-- | Command info and description
parserInfo :: ParserInfo Options
parserInfo = O.info
          (  O.helper <*> parseOptions)
          (  O.fullDesc
          <> O.progDesc "Populate Dhall value given its Dhall type (schema) from a JSON expression"
          )

-- | All the command arguments and options
data Options = Options
    { version    :: Bool
    , schema     :: Text
    , conversion :: Conversion
    } deriving Show

-- | Parser for all the command arguments and options
parseOptions :: Parser Options
parseOptions = Options <$> parseVersion
                       <*> parseSchema
                       <*> parseConversion
  where
    parseSchema  =  O.strArgument
                 (  O.metavar "SCHEMA"
                 <> O.help "Dhall type expression (schema)"
                 )
    parseVersion =  O.switch
                 (  O.long "version"
                 <> O.short 'V'
                 <> O.help "Display version"
                 )

-- | JSON-to-dhall translation options
data Conversion = Conversion
    { strictRecs  :: Bool
    , noKeyValArr :: Bool
    , noKeyValMap :: Bool
    , unions      :: UnionConv
    } deriving Show

data UnionConv = UFirst | UNone | UStrict deriving (Show, Read, Eq)

defaultConversion :: Conversion
defaultConversion =  Conversion
    { strictRecs  = False
    , noKeyValArr = False
    , noKeyValMap = False
    , unions      = UFirst
    }

-- | Parser for command options related to the conversion method
parseConversion :: Parser Conversion
parseConversion = Conversion <$> parseStrict
                             <*> parseKVArr
                             <*> parseKVMap
                             <*> parseUnion
  where
    parseStrict =  O.switch
                (  O.long "records-strict"
                <> O.help "Parse all fields in records"
                )
    parseKVArr  =  O.switch
                (  O.long "no-keyval-arrays"
                <> O.help "Disable conversion of key-value arrays to records"
                )
    parseKVMap  =  O.switch
                (  O.long "no-keyval-maps"
                <> O.help "Disable conversion of homogeneous map objects to association lists"
                )

-- | Parser for command options related to treating union types
parseUnion :: Parser UnionConv
parseUnion =
        uFirst
    <|> uNone
    <|> uStrict
    <|> pure UFirst -- defaulting to UFirst
  where
    uFirst  =  O.flag' UFirst
            (  O.long "unions-first"
            <> O.help "The first value with the matching type (succefully parsed all the way down the tree) is accepted, even if not the only posible match. (DEFAULT)"
            )
    uNone   =  O.flag' UNone
            (  O.long "unions-none"
            <> O.help "Unions not allowed"
            )
    uStrict =  O.flag' UStrict
            (  O.long "unions-strict"
            <> O.help "Error if more than one union values match the type (and parse successfully)"
            )


-- ----------
-- Main
-- ----------

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    Options {..} <- O.execParser parserInfo

    when version $ do
      putStrLn (showVersion Meta.version)
      System.Exit.exitSuccess

    handle $ do
        stdin <- BSL8.getContents
        value :: A.Value <- case A.eitherDecode stdin of
          Left err -> throwIO (userError err)
          Right v -> pure v

        expr <- typeCheckSchemaExpr =<< resolveSchemaExpr schema

        case dhallFromJSON conversion expr value of
          Left err -> throwIO err
          Right res -> Text.putStr (D.pretty res)

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure


-- ----------
-- Conversion
-- ----------

-- | The 'Expr' type concretization used throughout this module
type ExprX = Expr Src X

-- | Parse schema code to a valid Dhall expression and check that its type is actually Type
resolveSchemaExpr :: Text  -- ^ type code (schema)
                  -> IO ExprX
resolveSchemaExpr code = do
    parsedExpression <-
      case Dhall.Parser.exprFromText "\n\ESC[1;31mSCHEMA\ESC[0m" code of
        Left  err              -> Control.Exception.throwIO err
        Right parsedExpression -> return parsedExpression
    D.normalize <$> Dhall.Import.load parsedExpression -- IO

{-| Check that the Dhall type expression actually has type 'Type'
>>> :set -XOverloadedStrings
>>> import Dhall.Core

>>> typeCheckSchemaExpr =<< resolveSchemaExpr "List Natural"
App List Natural

>>> typeCheckSchemaExpr =<< resolveSchemaExpr "+1"
*** Exception:
Error: Schema expression is succesfully parsed but has Dhall type:
Integer
Expected Dhall type: Type
Parsed expression: +1
-}
typeCheckSchemaExpr :: MonadCatch m
                    => ExprX -> m ExprX
typeCheckSchemaExpr expr =
  case D.typeOf expr of -- check if the expression has type
    Left  err -> throwM $ TypeError err
    Right t   -> case t of -- check if the expression has type Type
      D.Const D.Type -> return expr
      _              -> throwM $ BadDhallType t expr

keyValMay :: A.Value -> Maybe (Text, A.Value)
keyValMay (A.Object o) = do
     A.String k <- HM.lookup "key" o
     v <- HM.lookup "value" o
     return (k, v)
keyValMay _ = Nothing


{-| The main conversion function. Traversing/zipping Dhall /type/ and Aeson value trees together to produce a Dhall /term/ tree, given 'Conversion' options:

>>> :set -XOverloadedStrings
>>> import qualified Dhall.Core as D
>>> import qualified Dhall.Map as Map
>>> import qualified Data.Aeson as A
>>> import qualified Data.HashMap.Strict as HM

>>> s = D.Record (Map.fromList [("foo", D.Integer)])
>>> v = A.Object (HM.fromList [("foo", A.Number 1)])
>>> dhallFromJSON defaultConversion s v
Right (RecordLit (fromList [("foo",IntegerLit 1)]))

-}
dhallFromJSON
  :: Conversion -> ExprX -> A.Value -> Either CompileError ExprX
dhallFromJSON (Conversion {..}) = loop
  where
    -- any ~> Union
    loop t@(D.Union tmMay) v = case unions of
      UNone -> Left $ ContainsUnion t
      _     -> case Map.traverseWithKey (const id) tmMay of
          Nothing -> undefined
          Just tm ->
            -- OLD-STYLE UNION:
            -- let f k a = D.UnionLit k <$> loop a v
            --                          <*> pure (Map.delete k tmMay)
            let f k a = D.App (D.Field t k) <$> loop a v
             in case rights . toList $ Map.mapWithKey f tm of
                  [ ]   -> Left $ Mismatch t v
                  [x]   -> Right x
                  xs@(x:_:_) -> case unions of
                      UStrict -> Left $ UndecidableUnion t v xs
                      UFirst  -> Right x
                      UNone   -> undefined -- can't happen

    -- object ~> Record
    loop (D.Record r) v@(A.Object o)
        | extraKeys <- HM.keys o \\ Map.keys r
        , strictRecs && not (null extraKeys)
        = Left (UnhandledKeys extraKeys (D.Record r) v)
        | otherwise
        = let f :: Text -> ExprX -> Either CompileError ExprX
              f k t | Just value <- HM.lookup k o
                    = loop t value
                    | App D.Optional t' <- t
                    = Right (App D.None t')
                    | otherwise
                    = Left (MissingKey k t v)
           in D.RecordLit <$> Map.traverseWithKey f r

    -- key-value list ~> Record
    loop t@(D.Record _) v@(A.Array a)
        | not noKeyValArr
        , os :: [A.Value] <- toList a
        , Just kvs <- traverse keyValMay os
        = loop t (A.Object $ HM.fromList kvs)
        | noKeyValArr
        = Left (NoKeyValArray t v)
        | otherwise
        = Left (Mismatch t v)

    -- object ~> List (key, value)
    loop t@(App D.List (D.Record r)) v@(A.Object o)
        | not noKeyValMap
        , ["mapKey", "mapValue"] == Map.keys r
        , Just D.Text == Map.lookup "mapKey" r
        , Just mapValue <- Map.lookup "mapValue" r
        , keyExprMap    :: Either CompileError (HM.HashMap Text ExprX)
                        <- traverse (loop mapValue) o
        = let f :: (Text, ExprX) -> ExprX
              f (key, val) = D.RecordLit ( Map.fromList
                  [ ("mapKey"  , D.TextLit (Chunks [] key))
                  , ("mapValue", val)
                  ] )
              recs :: Either CompileError (Dhall.Seq ExprX)
              recs = fmap f . Seq.fromList . HM.toList <$> keyExprMap
              typeAnn = if HM.null o then Just mapValue else Nothing
           in D.ListLit typeAnn <$> recs
        | noKeyValMap
        = Left (NoKeyValMap t v)
        | otherwise
        = Left (Mismatch t v)

    -- array ~> List
    loop (App D.List t) (A.Array a)
        = let f :: [ExprX] -> ExprX
              f es = D.ListLit
                       (if null es then Just t else Nothing)
                       (Seq.fromList es)
           in f <$> traverse (loop t) (toList a)

    -- number ~> Integer
    loop D.Integer (A.Number x)
        | Right n <- floatingOrInteger x :: Either Double Integer
        = Right (D.IntegerLit n)
        | otherwise
        = Left (Mismatch D.Integer (A.Number x))

    -- number ~> Natural
    loop D.Natural (A.Number x)
        | Right n <- floatingOrInteger x :: Either Double Dhall.Natural
        , n >= 0
        = Right (D.NaturalLit n)
        | otherwise
        = Left (Mismatch D.Natural (A.Number x))

    -- number ~> Double
    loop D.Double (A.Number x)
        = Right (D.DoubleLit $ toRealFloat x)

    -- string ~> Text
    loop D.Text (A.String t)
        = Right (D.TextLit (Chunks [] t))

    -- bool ~> Bool
    loop D.Bool (A.Bool t)
        = Right (D.BoolLit t)

    -- null ~> Optional
    loop (App D.Optional expr) A.Null
        = Right $ App D.None expr

    -- value ~> Optional
    loop (App D.Optional expr) value
        = D.Some <$> loop expr value

    -- fail
    loop expr value
        = Left (Mismatch expr value)


-- ----------
-- EXCEPTIONS
-- ----------

red, purple, green
    :: (Monoid a, Data.String.IsString a) => a -> a
red    s = "\ESC[1;31m" <> s <> "\ESC[0m" -- bold
purple s = "\ESC[1;35m" <> s <> "\ESC[0m" -- bold
green  s = "\ESC[0;32m" <> s <> "\ESC[0m" -- plain

showExpr :: ExprX   -> String
showExpr dhall = Text.unpack (D.pretty dhall)

showJSON :: A.Value -> String
showJSON value = BSL8.unpack (encodePretty value)

data CompileError
  -- Dhall shema
  = TypeError (D.TypeError Src X)
  | BadDhallType
      ExprX -- Expression type
      ExprX -- Whole expression
  -- generic mismatch (fallback)
  | Mismatch
      ExprX   -- Dhall expression
      A.Value -- Aeson value
  -- record specific
  | MissingKey     Text  ExprX A.Value
  | UnhandledKeys [Text] ExprX A.Value
  | NoKeyValArray        ExprX A.Value
  | NoKeyValMap          ExprX A.Value
  -- union specific
  | ContainsUnion        ExprX
  | UndecidableUnion     ExprX A.Value [ExprX]

instance Show CompileError where
  show = let prefix = red "\nError: "
          in \case
    TypeError e -> show e

    BadDhallType t e -> prefix
      <> "Schema expression is succesfully parsed but has Dhall type:\n"
      <> showExpr t <> "\nExpected Dhall type: Type"
      <> "\nParsed expression: "
      <> showExpr e <> "\n"

    ContainsUnion e -> prefix
      <> "Dhall type expression contains union type:\n"
      <> showExpr e <> "\nwhile it is forbidden by option "
      <> green "--unions-none\n"

    UndecidableUnion e v xs -> prefix
      <> "More than one union component type matches JSON value"
      <> "\n\nDhall:\n" <> showExpr e
      <> "\n\nJSON:\n"  <> showJSON v
      <> "\n\nPossible matches:\n\n" -- Showing all the allowed matches
      <> Text.unpack (Text.intercalate sep $ D.pretty <$> xs)
        where sep = red "\n--------\n" :: Text

    Mismatch e v -> prefix
      <> "Dhall type expression and json value do not match:"
      <> "\n\nDhall:\n" <> showExpr e
      <> "\n\nJSON:\n"  <> showJSON v
      <> "\n"

    MissingKey k e v -> prefix
      <> "Key " <> purple (Text.unpack k) <> ", expected by Dhall type:\n"
      <> showExpr e
      <> "\nis not present in JSON object:\n"
      <> showJSON v <> "\n"

    UnhandledKeys ks e v -> prefix
      <> "Key(s) " <> purple (Text.unpack (Text.intercalate ", " ks))
      <> " present in the JSON object but not in the corresponding Dhall record. This is not allowed in presence of "
      <> green "--records-strict" <> " flag:"
      <> "\n\nDhall:\n" <> showExpr e
      <> "\n\nJSON:\n"  <> showJSON v
      <> "\n"

    NoKeyValArray e v -> prefix
      <> "JSON (key-value) arrays cannot be converted to Dhall records under "
      <> green "--no-keyval-arrays" <> " flag"
      <> "\n\nDhall:\n" <> showExpr e
      <> "\n\nJSON:\n"  <> showJSON v
      <> "\n"

    NoKeyValMap e v -> prefix
      <> "Homogeneous JSON map objects cannot be converted to Dhall association lists under "
      <> green "--no-keyval-arrays" <> " flag"
      <> "\n\nDhall:\n" <> showExpr e
      <> "\n\nJSON:\n"  <> showJSON v
      <> "\n"

instance Exception CompileError
