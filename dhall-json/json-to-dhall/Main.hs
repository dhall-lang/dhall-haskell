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

import qualified Control.Exception
import           Control.Exception (Exception, SomeException, throwIO)
import           Control.Monad (when)
import qualified Data.Aeson as A
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Monoid ((<>))
import qualified Data.Text.IO as Text
import           Data.Version (showVersion)
import qualified GHC.IO.Encoding
import qualified Options.Applicative as O
import           Options.Applicative (ParserInfo)
import qualified System.Exit
import qualified System.IO

import qualified Dhall.Core as D
import           Dhall.JSONToDhall

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

-- ----------
-- JSON
-- ----------

showJSON :: A.Value -> String
showJSON value = BSL8.unpack (encodePretty value)

data JSONCompileError = JSONCompileError CompileError

instance Show JSONCompileError where
    show (JSONCompileError e) = showCompileError "JSON" showJSON e

instance Exception JSONCompileError

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

        expr <- typeCheckSchemaExpr JSONCompileError =<< resolveSchemaExpr schema

        case dhallFromJSON conversion expr value of
          Left err -> throwIO $ JSONCompileError err
          Right res -> Text.putStr (D.pretty res)

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
