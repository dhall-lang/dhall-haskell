-- | This module contains the implementation of the @dhall repl@ subcommand

{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module Dhall.Repl
    ( -- * Repl
      repl
    ) where

import Control.Exception ( SomeException(SomeException), displayException, throwIO )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.State.Class ( MonadState, get, modify )
import Control.Monad.State.Strict ( evalStateT )
import Data.List ( foldl', isPrefixOf )
import Dhall.Binary (StandardVersion(..))
import Dhall.Import (standardVersion)
import Dhall.Pretty (CharacterSet(..))
import Lens.Family (set)
import System.Console.Haskeline (Interrupt(..))
import System.Environment ( getEnvironment )

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty ( renderIO )
import qualified Dhall
import qualified Dhall.Binary
import qualified Dhall.Context
import qualified Dhall.Core
import qualified Dhall.Core as Dhall ( Var(V), Expr, normalize )
import qualified Dhall.Pretty
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Import as Dhall
import qualified Dhall.Map as Map
import qualified Dhall.Parser as Dhall
import qualified Dhall.TypeCheck as Dhall
import qualified System.Console.ANSI
import qualified System.Console.Haskeline.MonadException as Haskeline
import qualified System.Console.Repline as Repline
import qualified System.IO

type Repl = Repline.HaskelineT (State.StateT Env IO)

-- | Implementation of the @dhall repl@ subcommand
repl :: CharacterSet -> Bool -> StandardVersion -> IO ()
repl characterSet explain _standardVersion =
    if explain then Dhall.detailed io else io
  where
    io =
      evalStateT
        ( Repline.evalRepl
            ( pure "⊢ " )
            ( dontCrash . eval )
            ( options )
            ( Just optionsPrefix )
            completer
            greeter
        )
        (emptyEnv { characterSet, explain, _standardVersion })


data Env = Env
  { envBindings      :: Dhall.Context.Context Binding
  , envIt            :: Maybe Binding
  , explain          :: Bool
  , characterSet     :: CharacterSet
  , _standardVersion :: StandardVersion
  }


emptyEnv :: Env
emptyEnv =
  Env
    { envBindings = Dhall.Context.empty
    , envIt = Nothing
    , explain = False
    , _standardVersion = Dhall.Binary.defaultStandardVersion
    , characterSet = Unicode
    }


data Binding = Binding
  { bindingExpr :: Dhall.Expr Dhall.Src Dhall.X
  , bindingType :: Dhall.Expr Dhall.Src Dhall.X
  }


envToContext :: Env -> Dhall.Context.Context Binding
envToContext Env{ envBindings, envIt } =
  case envIt of
    Nothing ->
      envBindings

    Just it ->
      Dhall.Context.insert "it" it envBindings


parseAndLoad
  :: ( MonadIO m, MonadState Env m )
  => String -> m ( Dhall.Expr Dhall.Src Dhall.X )
parseAndLoad src = do
  env <-
    get

  parsed <-
    case Dhall.exprFromText "(stdin)" ( Text.pack src ) of
      Left e ->
        liftIO ( throwIO e )

      Right a ->
        return a

  let status =
        set standardVersion (_standardVersion env) (Dhall.emptyStatus ".")

  liftIO ( State.evalStateT (Dhall.loadWith parsed) status )


eval :: ( MonadIO m, MonadState Env m ) => String -> m ()
eval src = do
  loaded <-
    parseAndLoad src

  exprType <-
    typeCheck loaded

  expr <-
    normalize loaded

  modify ( \e -> e { envIt = Just ( Binding expr exprType ) } )

  output System.IO.stdout expr



typeOf :: ( MonadIO m, MonadState Env m ) => [String] -> m ()
typeOf [] =
  liftIO ( putStrLn ":type requires an argument to check the type of" )


typeOf srcs = do
  loaded <-
    parseAndLoad ( unwords srcs )

  exprType <-
    typeCheck loaded

  exprType' <-
    normalize exprType

  output System.IO.stdout exprType'



normalize
  :: MonadState Env m
  => Dhall.Expr Dhall.Src Dhall.X -> m ( Dhall.Expr t Dhall.X )
normalize e = do
  env <-
    get

  return
    ( Dhall.normalize
        ( foldl'
            ( \a (k, Binding { bindingType, bindingExpr }) ->
                Expr.Let (pure (Dhall.Core.Binding k (Just bindingType) bindingExpr)) a
            )
            e
            ( Dhall.Context.toList ( envToContext env ) )
        )
    )


typeCheck
  :: ( MonadIO m, MonadState Env m )
  => Dhall.Expr Dhall.Src Dhall.X -> m ( Dhall.Expr Dhall.Src Dhall.X )
typeCheck expr = do
  env <-
    get

  let wrap = if explain env then Dhall.detailed else id

  case Dhall.typeWith ( bindingType <$> envToContext env ) expr of
    Left e ->
      liftIO ( wrap (throwIO e) )

    Right a ->
      return a


addBinding :: ( MonadIO m, MonadState Env m ) => [String] -> m ()
addBinding (k : "=" : srcs) = do
  let
    varName =
      Text.pack k

  loaded <-
    parseAndLoad ( unwords srcs )

  t <-
    typeCheck loaded

  expr <-
    normalize loaded

  modify
    ( \e ->
        e
          { envBindings =
              Dhall.Context.insert
                varName
                Binding { bindingType = t, bindingExpr = expr }
                ( envBindings e )
          }
    )

  output
    System.IO.stdout
    ( Expr.Annot ( Expr.Var ( Dhall.V varName 0 ) ) t )

addBinding _ =
  liftIO ( fail ":let should be of the form `:let x = y`" )

saveBinding :: ( MonadIO m, MonadState Env m ) => [String] -> m ()
saveBinding (file : "=" : tokens) = do
  loadedExpression <- parseAndLoad (unwords tokens)

  _ <- typeCheck loadedExpression

  normalizedExpression <- normalize loadedExpression

  env <- get

  let handler handle =
          State.evalStateT (output handle normalizedExpression) env

  liftIO (System.IO.withFile file System.IO.WriteMode handler)
saveBinding _ = fail ":save should be of the form `:save x = y`"

cmdQuit :: ( MonadIO m, MonadState Env m ) => [String] -> m ()
cmdQuit _ = do
  liftIO (putStrLn "Goodbye.")
  liftIO (throwIO Interrupt)


optionsPrefix :: Char
optionsPrefix = ':'


options
  :: ( Haskeline.MonadException m, MonadIO m, MonadState Env m )
  => Repline.Options m
options =
  [ ( "type", dontCrash . typeOf )
  , ( "let", dontCrash . addBinding )
  , ( "save", dontCrash . saveBinding )
  , ( "quit", cmdQuit )
  ]


completer
  :: (Monad m, MonadIO m, MonadState Env m)
  => Repline.CompleterStyle m
completer =
  Repline.Prefix
    dhallCompleter
    [ optionsCompleter ]


optionsCompleter :: Monad m => (String, Repline.CompletionFunc m)
optionsCompleter =
  ( pure optionsPrefix
  , Repline.listCompleter $ (optionsPrefix :) . fst <$> (options :: Repline.Options Repl)
  )


dhallCompleter
  :: (Monad m, MonadIO m, MonadState Env m)
  => Repline.CompletionFunc m
dhallCompleter ctx@(reversedPrefix, _)

  -- Complete file paths
  | any (`isPrefixOf` word) [ "/", "./", "../", "~/" ]
  = Repline.fileCompleter ctx

  -- Complete environment variables
  | "env:" `isPrefixOf` word
  = do

    envs <- fmap fst <$> liftIO getEnvironment

    Repline.listCompleter (map ("env:" ++) envs) ctx

  -- Complete record fields and union alternatives
  | '.' `elem` word
  = do
    Env { envBindings } <- get

    let var:subFields = Text.split (== '.') (Text.pack word)

    case Dhall.Context.lookup var 0 envBindings of

      Nothing -> noComplete

      Just binding -> do
        let candidates = algebraicComplete subFields (bindingExpr binding)
        Repline.listCompleter (Text.unpack . (var <>) <$> candidates) ctx

  | otherwise
  = noComplete

  where
    noComplete = Repline.listCompleter [] ctx

    -- The previous word using the separators to delimit what a word is
    word = reverse $ takeWhile (`notElem` separators) reversedPrefix

    -- Separators that can be found on the left of something we want to
    -- autocomplete
    separators :: String
    separators = " \t[(,=+*&|}#?>"


    algebraicComplete :: [Text.Text] -> Dhall.Expr Dhall.Src Dhall.X -> [Text.Text]
    algebraicComplete subFields expr =
      let keys = fmap ("." <>) . Map.keys

          withMap m
            | [] <- subFields   = keys m
            | f:fs <- subFields =
              maybe
                (keys m)
                (fmap (("." <> f) <>) . algebraicComplete fs)
                (Map.lookup f m)

      in  case expr of
            Dhall.Core.Record       m -> withMap m
            Dhall.Core.RecordLit    m -> withMap m
            Dhall.Core.Union        m -> withMap m
            Dhall.Core.UnionLit _ _ m -> withMap m
            _                         -> []


greeter :: MonadIO m => m ()
greeter =
  return ()


dontCrash :: ( MonadIO m, Haskeline.MonadException m ) => m () -> m ()
dontCrash m =
  Haskeline.catch
    m
    ( \ e@SomeException{} -> liftIO ( putStrLn ( displayException e ) ) )


output
    :: (Pretty.Pretty a, MonadState Env m, MonadIO m)
    => System.IO.Handle -> Dhall.Expr s a -> m ()
output handle expr = do
  Env { characterSet } <- get

  liftIO (System.IO.hPutStrLn handle "")  -- Visual spacing

  let stream =
          Pretty.layoutSmart Dhall.Pretty.layoutOpts
              (Dhall.Pretty.prettyCharacterSet characterSet expr)

  supportsANSI <- liftIO (System.Console.ANSI.hSupportsANSI handle)
  let ansiStream =
          if supportsANSI
          then fmap Dhall.Pretty.annToAnsiStyle stream
          else Pretty.unAnnotateS stream

  liftIO (Pretty.renderIO handle ansiStream)
  liftIO (System.IO.hPutStrLn handle "") -- Pretty printing doesn't end with a new line

  liftIO (System.IO.hPutStrLn handle "")  -- Visual spacing
