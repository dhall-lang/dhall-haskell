{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module Main ( main ) where 

import Control.Exception ( SomeException(SomeException), displayException, throwIO )
import Control.Monad.State.Strict ( evalStateT )
import Control.Monad.State.Class ( MonadState, get, modify )
import Control.Monad.IO.Class ( MonadIO, liftIO )

import qualified Data.Map.Strict as StrictMap
import qualified Data.Text.Lazy as LazyText 
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty ( renderIO )
import qualified Dhall
import qualified Dhall.Context
import qualified Dhall.Core as Dhall ( Var(V), Expr, normalize, subst )
import qualified Dhall.Pretty
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Import as Dhall
import qualified Dhall.Parser as Dhall
import qualified Dhall.TypeCheck as Dhall
import qualified System.Console.Haskeline.MonadException as Haskeline
import qualified System.Console.Repline as Repline
import qualified System.IO
import qualified Text.Trifecta.Delta as Trifecta


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
  { envBindings :: StrictMap.Map Dhall.Text Binding
  }


emptyEnv :: Env
emptyEnv =
  Env
    { envBindings = mempty
    }


data Binding = Binding
  { bindingExpr :: Dhall.Expr Dhall.Src Dhall.X
  , bindingType :: Dhall.Expr Dhall.Src Dhall.X
  }


envToContext :: Env -> Dhall.Context.Context ( Dhall.Expr Dhall.Src Dhall.X )
envToContext Env{ envBindings } =
  StrictMap.foldlWithKey'
    ( \ctx k b -> Dhall.Context.insert k ( bindingType b ) ctx )
    Dhall.Context.empty
    envBindings


parseAndLoad
  :: ( MonadIO m, MonadState Env m )
  => String -> m ( Dhall.Expr Dhall.Src Dhall.X )
parseAndLoad src = do
  parsed <-
    case Dhall.exprFromText ( Trifecta.Columns 0 0 ) ( LazyText.pack src ) of
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

  modify
    ( \e ->
        e
          { envBindings =
              StrictMap.insert "it" ( Binding expr exprType ) ( envBindings e )
          }
    )

  output expr
    


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

  output ( Expr.Annot loaded exprType' )



normalize
  :: MonadState Env m
  => Dhall.Expr Dhall.Src Dhall.X -> m ( Dhall.Expr t Dhall.X )
normalize e = do
  env <-
    get

  return
    ( Dhall.normalize
        ( StrictMap.foldlWithKey'
            ( \a k expr ->
                Dhall.subst ( Dhall.V k 0 ) ( bindingExpr expr ) a
            )
            e
            ( envBindings env )
        )
    )


typeCheck
  :: ( MonadIO m, MonadState Env m )
  => Dhall.Expr Dhall.Src Dhall.X -> m ( Dhall.Expr Dhall.Src Dhall.X )
typeCheck expr = do
  env <-
    get

  case Dhall.typeWith ( envToContext env ) expr of
    Left e ->
      liftIO ( throwIO e )

    Right a ->
      return a


addBinding :: ( MonadIO m, MonadState Env m ) => [String] -> m ()
addBinding (k : "=" : srcs) = do 
  let
    varName =
      LazyText.pack k
  
  loaded <-
    parseAndLoad ( unwords srcs )

  expr <-
    normalize loaded

  t <-
    typeCheck expr

  modify
    ( \e ->
        e
          { envBindings =
              StrictMap.insert
                varName
                ( Binding { bindingType = t, bindingExpr = expr } )
                ( envBindings e )
          }
    )

  output 
    ( Expr.Annot ( Expr.Var ( Dhall.V varName 0 ) ) t )

addBinding _ =
  error ":let should be of the form `:let x = y`"


options
  :: ( Haskeline.MonadException m, MonadIO m, MonadState Env m )
  => Repline.Options m
options =
  [ ( "type", dontCrash . typeOf )
  , ( "let", dontCrash . addBinding )
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


output :: ( Pretty.Pretty a, MonadIO m ) => Dhall.Expr s a -> m ()
output expr = do
  let
    opts =
      Pretty.defaultLayoutOptions
        { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }

  liftIO
    ( Pretty.renderIO
        System.IO.stdout
        ( fmap
            Dhall.Pretty.annToAnsiStyle
            ( Pretty.layoutSmart opts ( Dhall.Pretty.prettyExpr expr ) )
        )
    )

  liftIO ( putStrLn "" ) -- Pretty printing doesn't end with a new line
