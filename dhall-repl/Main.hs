{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module Main ( main ) where

import Control.Exception ( SomeException(SomeException), displayException, throwIO )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.State.Class ( MonadState, get, modify )
import Control.Monad.State.Strict ( evalStateT )
import Data.List ( foldl' )

import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty ( renderIO )
import qualified Dhall.Context
import qualified Dhall.Core as Dhall ( Var(V), Expr, normalize )
import qualified Dhall.Pretty
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Import as Dhall
import qualified Dhall.Parser as Dhall
import qualified Dhall.TypeCheck as Dhall
import qualified System.Console.ANSI
import qualified System.Console.Haskeline.MonadException as Haskeline
import qualified System.Console.Repline as Repline
import qualified System.IO


main :: IO ()
main =
  evalStateT
    ( Repline.evalRepl
        "⊢ "
        ( dontCrash . eval )
        options
        ( Repline.Word completer )
        greeter
    )
    emptyEnv


data Env = Env
  { envBindings :: Dhall.Context.Context Binding
  , envIt :: Maybe Binding
  }


emptyEnv :: Env
emptyEnv =
  Env
    { envBindings = Dhall.Context.empty
    , envIt = Nothing
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
  parsed <-
    case Dhall.exprFromText "(stdin)" ( Text.pack src ) of
      Left e ->
        liftIO ( throwIO e )

      Right a ->
        return a

  liftIO ( Dhall.load parsed )


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
                Expr.Let k ( Just bindingType ) bindingExpr a
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

  case Dhall.typeWith ( bindingType <$> envToContext env ) expr of
    Left e ->
      liftIO ( throwIO e )

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

  let handler handle = output handle normalizedExpression

  liftIO (System.IO.withFile file System.IO.WriteMode handler)
saveBinding _ = fail ":save should be of the form `:save x = y`"


options
  :: ( Haskeline.MonadException m, MonadIO m, MonadState Env m )
  => Repline.Options m
options =
  [ ( "type", dontCrash . typeOf )
  , ( "let", dontCrash . addBinding )
  , ( "save", dontCrash . saveBinding )
  ]


completer :: Monad m => Repline.WordCompleter m
completer _ =
  return []


greeter :: MonadIO m => m ()
greeter =
  return ()


dontCrash :: ( MonadIO m, Haskeline.MonadException m ) => m () -> m ()
dontCrash m =
  Haskeline.catch
    m
    ( \ e@SomeException{} -> liftIO ( putStrLn ( displayException e ) ) )


output
    :: (Pretty.Pretty a, MonadIO m)
    => System.IO.Handle -> Dhall.Expr s a -> m ()
output handle expr = do
  liftIO (System.IO.hPutStrLn handle "")  -- Visual spacing

  let opts =
          Pretty.defaultLayoutOptions
              { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }

  let stream = Pretty.layoutSmart opts (Dhall.Pretty.prettyExpr expr)
  supportsANSI <- liftIO (System.Console.ANSI.hSupportsANSI handle)
  let ansiStream =
          if supportsANSI
          then fmap Dhall.Pretty.annToAnsiStyle stream
          else Pretty.unAnnotateS stream

  liftIO (Pretty.renderIO handle ansiStream)
  liftIO (System.IO.hPutStrLn handle "") -- Pretty printing doesn't end with a new line

  liftIO (System.IO.hPutStrLn handle "")  -- Visual spacing
