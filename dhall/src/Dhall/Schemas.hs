{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

{-| This module contains the implementation of the @dhall rewrite-with-schemas@
    subcommand
-}

module Dhall.Schemas
    ( -- | Schemas
      schemasCommand
    , Schemas(..)
    , simplifyUsingSchemas
    , SchemasError(..)
    ) where

import Control.Applicative (empty)
import Control.Exception (Exception)
import Data.List ((\\))
import Data.Monoid
import Data.Text (Text)
import Data.Void (Void)
import Dhall.Map (Map)
import Dhall.Src (Src)
import Dhall.Syntax (Binding(..), Expr(..), Import, Var(..))
import Dhall.Pretty (CharacterSet(..))
import Dhall.Util
    (Censor(..), CheckFailed(..), Header(..), Input(..), OutputMode(..))

import qualified Control.Exception                         as Exception
import qualified Data.Maybe                                as Maybe
import qualified Data.Text.IO                              as Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty.Text
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty.Terminal
import qualified Data.Void                                 as Void
import qualified Dhall.Core                                as Core
import qualified Dhall.Import                              as Import
import qualified Dhall.Map                                 as Map
import qualified Dhall.Normalize                           as Normalize
import qualified Dhall.Optics                              as Optics
import qualified Dhall.Parser                              as Parser
import qualified Dhall.Pretty
import qualified Dhall.Syntax                              as Syntax
import qualified Dhall.TypeCheck                           as TypeCheck
import qualified Dhall.Util                                as Util
import qualified System.AtomicWrite.Writer.LazyText        as AtomicWrite
import qualified System.Console.ANSI                       as ANSI
import qualified System.IO                                 as IO

-- | Arguments to the @schemas@ subcommand
data Schemas = Schemas
    { characterSet :: CharacterSet
    , censor       :: Censor
    , input        :: Input
    , outputMode   :: OutputMode
    , schemas      :: Text
    }

-- | Implementation of the @dhall schemas@ subcommand
schemasCommand :: Schemas -> IO ()
schemasCommand Schemas{..} = do
    originalText <- case input of
        InputFile file -> Text.IO.readFile file
        StandardInput  -> Text.IO.getContents

    (Header header, expression) <- Util.getExpressionAndHeaderFromStdinText censor originalText

    schemasRecord <- Core.throws (Parser.exprFromText "(schemas)" schemas)

    schemasExpression <- simplifyUsingSchemas schemasRecord expression

    let docStream =
            Dhall.Pretty.layout
                (   Pretty.pretty header
                <>  Dhall.Pretty.prettyCharacterSet characterSet schemasExpression
                <>  "\n"
                )

    let schemasText = Pretty.Text.renderStrict docStream

    case outputMode of
        Write -> do
            case input of
                InputFile file -> do
                    if originalText == schemasText
                        then return ()
                        else AtomicWrite.atomicWriteFile
                                file
                                (Pretty.Text.renderLazy docStream)
                StandardInput -> do
                    supportsANSI <- ANSI.hSupportsANSI IO.stdout

                    Pretty.Terminal.renderIO
                        IO.stdout
                        (if supportsANSI
                            then fmap Dhall.Pretty.annToAnsiStyle docStream
                            else Pretty.unAnnotateS docStream)

        Check -> do
            if originalText == schemasText
                then return ()
                else do
                    let command = "rewrite-with-schemas"

                    let modified = "rewritten"

                    Exception.throwIO CheckFailed{..}

decodeSchema
    :: Expr Src Void
    -> Maybe (Map Text (Expr Src Void), Map Text (Expr Src Void))
decodeSchema
    (RecordLit [ ("Type", Record _Type), ("default", RecordLit _default) ]) =
    Just (_Type, _default)
decodeSchema _ =
    Nothing

decodeSchemas
    :: Expr Src Void
    -> Maybe (Map Text (Map Text (Expr Src Void), Map Text (Expr Src Void)))
decodeSchemas (RecordLit keyValues) = traverse decodeSchema keyValues
decodeSchemas  _                    = Nothing

-- | Simplify a Dhall expression using a record of schemas
simplifyUsingSchemas
    :: Expr Src Import
    -- ^ Record of schemas
    -> Expr Src Import
    -- ^ Expression to simplify using the supplied schemas
    -> IO (Expr Src Import)
simplifyUsingSchemas _schemas expression = do
    resolvedSchemas    <- Import.load _schemas
    resolvedExpression <- Import.load expression

    _ <- Core.throws (TypeCheck.typeOf resolvedSchemas)
    _ <- Core.throws (TypeCheck.typeOf resolvedExpression)

    let normalizedSchemas    = Normalize.normalize resolvedSchemas
    let normalizedExpression = Normalize.normalize resolvedExpression

    decodedSchemas <- case decodeSchemas normalizedSchemas of
        Just decodedSchemas -> return decodedSchemas
        Nothing             -> Exception.throwIO NotASchemaRecord

    let schemasBinding value = Binding{..}
          where
            variable   = "schemas"
            annotation = Nothing

            bindingSrc0 = Nothing
            bindingSrc1 = Nothing
            bindingSrc2 = Nothing

    let schemasRewrite subExpression =
            (Maybe.fromMaybe subExpression . Maybe.listToMaybe) $ do
                (name, (_Type, _default)) <- Map.toList decodedSchemas

                defaultedRecord <- case subExpression of
                    RecordLit keyValues -> do
                        let defaultedKeyValues =
                                Map.fromList (Map.toList keyValues \\ Map.toList _default)

                        return (RecordLit defaultedKeyValues)

                    _ -> empty

                case TypeCheck.typeOf (Annot (Let (schemasBinding resolvedSchemas) subExpression) (Record _Type)) of
                    Left  _ -> empty
                    Right _ -> return ()

                return (RecordCompletion (Field "schemas" name) defaultedRecord)

    let rewrittenExpression =
            fmap Void.absurd (Optics.transformOf Syntax.subExpressions schemasRewrite normalizedExpression)

    let usesSchema = (not . null) $ do
            Var (V "schemas" 0) <- Optics.universeOf Syntax.subExpressions rewrittenExpression
            return ()

    if usesSchema
        then return (Let (schemasBinding _schemas) rewrittenExpression)
        else return rewrittenExpression

data SchemasError = NotASchemaRecord
    deriving (Exception)

instance Show SchemasError where
    show NotASchemaRecord =
        Util._ERROR <> ": The --record argument is not a record of schemas"
