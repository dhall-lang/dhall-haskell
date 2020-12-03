{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains the implementation of the @dhall format@ subcommand

module Dhall.Format
    ( -- * Format
      Format(..)
    , format
    ) where

import Data.Foldable (for_)
import Data.Maybe    (fromMaybe)
import Dhall.Pretty  (CharacterSet (..), annToAnsiStyle)
import Dhall.Src     (Src (..))
import Dhall.Syntax
    ( Chunks (..)
    , Expr (..)
    , PreferAnnotation (PreferFromWith)
    , RecordField (recordFieldValue)
    )
import Dhall.Util
    ( Censor
    , CheckFailed (..)
    , Header (..)
    , OutputMode (..)
    , PossiblyTransitiveInput (..)
    , Transitivity (..)
    )

import qualified Control.Exception
import qualified Data.Text                                 as Text
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty.Text
import qualified Dhall.Import
import qualified Dhall.Pretty
import qualified Dhall.Util
import qualified System.AtomicWrite.Writer.LazyText        as AtomicWrite.LazyText
import qualified System.Console.ANSI
import qualified System.FilePath
import qualified System.IO

-- | Arguments to the `format` subcommand
data Format = Format
    { chosenCharacterSet :: Maybe CharacterSet
    , censor             :: Censor
    , input              :: PossiblyTransitiveInput
    , outputMode         :: OutputMode
    }

-- | Implementation of the @dhall format@ subcommand
format :: Format -> IO ()
format (Format { input = input0, ..}) = go input0
  where
    go input = do
        let directory = case input of
                NonTransitiveStandardInput ->
                    "."
                PossiblyTransitiveInputFile file _ ->
                    System.FilePath.takeDirectory file

        let status = Dhall.Import.emptyStatus directory

        let layoutHeaderAndExpr (Header header, expr) =
                let -- When the user does not chose an explicit character set,
                    -- detect it from the expression
                    characterSet = fromMaybe (detectCharacterSet expr) chosenCharacterSet
                in
                Dhall.Pretty.layout
                    (   Pretty.pretty header
                    <>  Dhall.Pretty.prettyCharacterSet characterSet expr
                    <>  "\n")

        (originalText, transitivity) <- case input of
            PossiblyTransitiveInputFile file transitivity -> do
                text <- Data.Text.IO.readFile file

                return (text, transitivity)

            NonTransitiveStandardInput -> do
                text <- Data.Text.IO.getContents

                return (text, NonTransitive)

        headerAndExpr@(_, parsedExpression) <- Dhall.Util.getExpressionAndHeaderFromStdinText censor originalText

        case transitivity of
            Transitive ->
                for_ parsedExpression $ \import_ -> do
                    maybeFilepath <- Dhall.Import.dependencyToFile status import_

                    for_ maybeFilepath $ \filepath ->
                        go (PossiblyTransitiveInputFile filepath Transitive)

            NonTransitive ->
                return ()

        let docStream = layoutHeaderAndExpr headerAndExpr

        let formattedText = Pretty.Text.renderStrict docStream

        case outputMode of
            Write ->
                case input of
                    PossiblyTransitiveInputFile file _ ->
                        if originalText == formattedText
                            then return ()
                            else AtomicWrite.LazyText.atomicWriteFile
                                    file
                                    (Pretty.Text.renderLazy docStream)

                    NonTransitiveStandardInput -> do
                        supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout

                        Pretty.Terminal.renderIO
                            System.IO.stdout
                            (if supportsANSI
                                then (fmap annToAnsiStyle docStream)
                                else (Pretty.unAnnotateS docStream))

            Check ->
                if originalText == formattedText
                    then return ()
                    else do
                        let command = "format"

                        let modified = "formatted"

                        Control.Exception.throwIO CheckFailed{..}

-- | Detect which character set is used for the syntax of an expression
-- If any parts of the expression uses the Unicode syntax, the whole expression
-- is deemed to be using the Unicode syntax.
detectCharacterSet :: Expr Src a -> CharacterSet
detectCharacterSet = go mempty
  where
    recurse = go ASCII

    -- Short-circuit traversal if Unicode encountered
    go Unicode = \_ -> Unicode
    go ASCII = \case
        Note Src{srcText} expr ->
            case expr of
                Lam _ _
                    | "λ" `Text.isInfixOf` srcText ||
                      "→" `Text.isInfixOf` srcText -> Unicode
                Pi _ _ _
                    | "∀" `Text.isInfixOf` srcText ||
                      "→" `Text.isInfixOf` srcText -> Unicode
                Prefer _ _ _
                    | "⫽" `Text.isInfixOf` srcText -> Unicode
                Combine _ _ _
                    | "∧" `Text.isInfixOf` srcText -> Unicode
                CombineTypes _ _
                    | "⩓" `Text.isInfixOf` srcText -> Unicode
                _ -> recurse expr

        -- Recurse down the AST
        Const _ -> ASCII
        Var _ -> ASCII
        Lam _ a -> recurse a
        Pi _ a b -> recurse a <> recurse b
        App f a -> recurse f <> recurse a
        Let _ a -> recurse a
        Annot a b -> recurse a <> recurse b
        Bool -> ASCII
        BoolLit _ -> ASCII
        BoolAnd a b -> recurse a <> recurse b
        BoolOr a b -> recurse a <> recurse b
        BoolEQ a b -> recurse a <> recurse b
        BoolNE a b -> recurse a <> recurse b
        BoolIf c a b -> recurse c <> recurse a <> recurse b
        Natural -> ASCII
        NaturalLit _ -> ASCII
        NaturalFold -> ASCII
        NaturalBuild -> ASCII
        NaturalIsZero -> ASCII
        NaturalEven -> ASCII
        NaturalOdd -> ASCII
        NaturalToInteger -> ASCII
        NaturalShow -> ASCII
        NaturalSubtract -> ASCII
        NaturalPlus a b -> recurse a <> recurse b
        NaturalTimes a b -> recurse a <> recurse b
        Integer -> ASCII
        IntegerLit _ -> ASCII
        IntegerClamp -> ASCII
        IntegerNegate -> ASCII
        IntegerToDouble -> ASCII
        IntegerShow -> ASCII
        Double -> ASCII
        DoubleLit _ -> ASCII
        DoubleShow -> ASCII
        Text -> ASCII
        TextLit (Chunks cs _) -> foldMap (recurse . snd) cs
        TextAppend a b -> recurse a <> recurse b
        TextReplace -> ASCII
        TextShow -> ASCII
        List -> ASCII
        ListLit a as -> foldMap recurse a <> foldMap recurse as
        ListAppend a b -> recurse a <> recurse b
        ListBuild -> ASCII
        ListFold -> ASCII
        ListLength -> ASCII
        ListHead -> ASCII
        ListLast -> ASCII
        ListIndexed -> ASCII
        ListReverse -> ASCII
        Optional -> ASCII
        Some a -> recurse a
        None -> ASCII
        Record fs -> foldMap (recurse . recordFieldValue) fs
        RecordLit fs -> foldMap (recurse . recordFieldValue) fs
        Union fs -> (foldMap . foldMap) recurse fs
        Combine _ a b -> recurse a <> recurse b
        CombineTypes a b -> recurse a <> recurse b
        Prefer (PreferFromWith w) a b -> recurse w <> recurse a <> recurse b
        Prefer _ a b -> recurse a <> recurse b
        RecordCompletion a b -> recurse a <> recurse b
        Merge a b t -> recurse a <> recurse b <> foldMap recurse t
        ToMap a t -> recurse a <> foldMap recurse t
        Field a _ -> recurse a
        Project a b -> recurse a <> foldMap recurse b
        Assert a -> recurse a
        Equivalent a b -> recurse a <> recurse b
        With a _ b -> recurse a <> recurse b
        ImportAlt a b -> recurse a <> recurse b
        Embed _ -> ASCII
