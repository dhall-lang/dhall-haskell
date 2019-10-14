-- | This module contains the implementation of the @dhall repl@ subcommand

{-# language FlexibleContexts  #-}
{-# language NamedFieldPuns    #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards   #-}

module Dhall.Repl
    ( -- * Repl
      repl
    ) where

import Control.Exception ( SomeException(SomeException), displayException, throwIO )
import Control.Monad ( forM_ )
import Control.Monad.Fail ( MonadFail )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.State.Class ( MonadState, get, modify )
import Control.Monad.State.Strict ( evalStateT )
-- For the MonadFail instance for StateT.
import Control.Monad.Trans.Instances ()
import Data.List ( isPrefixOf, nub )
import Data.Maybe ( mapMaybe )
import Data.Semigroup ((<>))
import Data.Text ( Text )
import Data.Void (Void)
import Dhall.Context (Context)
import Dhall.Import (hashExpressionToCode)
import Dhall.Src (Src)
import Dhall.Pretty (CharacterSet(..))
import System.Console.Haskeline (Interrupt(..))
import System.Console.Haskeline.Completion ( Completion, simpleCompletion )
import System.Directory ( getDirectoryContents )
import System.Environment ( getEnvironment )

import qualified Control.Monad.Fail as Fail
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.HashSet
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty ( renderIO )
import qualified Dhall
import qualified Dhall.Context
import qualified Dhall.Core
import qualified Dhall.Core as Dhall ( Var(V), Expr, normalize )
import qualified Dhall.Pretty
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Import as Dhall
import qualified Dhall.Map as Map
import qualified Dhall.Parser as Dhall
import qualified Dhall.TypeCheck as Dhall
import qualified Dhall.Version as Meta
import qualified System.Console.ANSI
import qualified System.Console.Haskeline.Completion as Haskeline
import qualified System.Console.Haskeline.MonadException as Haskeline
import qualified System.Console.Repline as Repline
import qualified System.IO

type Repl = Repline.HaskelineT (State.StateT Env IO)

-- | Implementation of the @dhall repl@ subcommand
repl :: CharacterSet -> Bool -> IO ()
repl characterSet explain =
    if explain then Dhall.detailed io else io
  where
    io =
      evalStateT
        ( Repline.evalRepl
            ( pure $ turnstile ++ " " )
            ( dontCrash . eval )
            options
            ( Just optionsPrefix )
            completer
            greeter
        )
        (emptyEnv { characterSet, explain })

    turnstile =
      case characterSet of
        Unicode -> "⊢"
        ASCII   -> "|-"

data Env = Env
  { envBindings      :: Dhall.Context.Context Binding
  , envIt            :: Maybe Binding
  , explain          :: Bool
  , characterSet     :: CharacterSet
  , outputHandle     :: Maybe System.IO.Handle
  }


emptyEnv :: Env
emptyEnv =
  Env
    { envBindings = Dhall.Context.empty
    , envIt = Nothing
    , explain = False
    , characterSet = Unicode
    , outputHandle = Just System.IO.stdout
    }


data Binding = Binding
  { bindingExpr :: Dhall.Expr Dhall.Src Void
  , bindingType :: Dhall.Expr Dhall.Src Void
  }


envToContext :: Env -> Dhall.Context.Context Binding
envToContext Env{ envBindings, envIt } =
  case envIt of
    Nothing ->
      envBindings

    Just it ->
      Dhall.Context.insert "it" it envBindings


parseAndLoad
  :: MonadIO m => String -> m ( Dhall.Expr Dhall.Src Void) 
parseAndLoad src = do
  parsed <-
    case Dhall.exprFromText "(stdin)" ( Text.pack src ) of
      Left e ->
        liftIO ( throwIO e )

      Right a ->
        return a

  let status = Dhall.emptyStatus "."

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

  output expr



typeOf :: ( MonadFail m, MonadIO m, MonadState Env m ) => [String] -> m ()
typeOf [] = Fail.fail ":type requires an argument to check the type of"

typeOf srcs = do
  loaded <-
    parseAndLoad ( unwords srcs )

  exprType <-
    typeCheck loaded

  output exprType


applyContext
    :: Context Binding
    -> Dhall.Expr Dhall.Src Void
    -> Dhall.Expr Dhall.Src Void
applyContext context expression =
    Dhall.Core.wrapInLets bindings expression
  where
    definitions = reverse $ Dhall.Context.toList context

    convertBinding (variable, Binding expr _) =
        Dhall.Core.Binding Nothing variable Nothing Nothing Nothing expr

    bindings = fmap convertBinding definitions

normalize
  :: MonadState Env m
  => Dhall.Expr Dhall.Src Void -> m ( Dhall.Expr t Void )
normalize e = do
  env <- get

  return (Dhall.normalize (applyContext (envToContext env) e))


typeCheck
  :: ( MonadIO m, MonadState Env m )
  => Dhall.Expr Dhall.Src Void -> m ( Dhall.Expr Dhall.Src Void )
typeCheck expression = do
  env <- get

  let wrap = if explain env then Dhall.detailed else id

  case Dhall.typeOf (applyContext (envToContext env) expression) of
    Left  e -> liftIO ( wrap (throwIO e) )
    Right a -> return a

-- Separate the equal sign to be its own word in order to simplify parsing
-- This is intended to be used with the options that require assignment
separateEqual :: [String] -> [String]
separateEqual [] =
    []
separateEqual (str₀ : ('=' : str₁) : strs) =
    str₀ : "=" : str₁ : strs
separateEqual (str : strs)
    | (str₀, '=' : str₁) <- break (== '=') str =
        str₀ : "=" : str₁ : strs
    | otherwise =
        str : strs

addBinding :: ( MonadFail m, MonadIO m, MonadState Env m ) => [String] -> m ()
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

  output ( Expr.Annot ( Expr.Var ( Dhall.V varName 0 ) ) t )

addBinding _ = Fail.fail ":let should be of the form `:let x = y`"

clearBindings :: (MonadFail m, MonadState Env m) => [String] -> m ()
clearBindings [] = modify adapt
  where
    adapt (Env {..}) = Env { envBindings = Dhall.Context.empty, ..}

clearBindings _ = Fail.fail ":clear takes no arguments"

hashBinding :: ( MonadFail m, MonadIO m, MonadState Env m ) => [String] -> m ()
hashBinding [] = Fail.fail ":hash should be of the form `:hash expr"
hashBinding tokens = do
  loadedExpression <- parseAndLoad (unwords tokens)

  _ <- typeCheck loadedExpression

  normalizedExpression <- normalize loadedExpression

  writeOutputHandle $ hashExpressionToCode normalizedExpression

saveFilePrefix :: FilePath
saveFilePrefix = ".dhall-repl"

-- | Find the index for the current _active_ dhall save file
currentSaveFileIndex :: MonadIO m => m (Maybe Int)
currentSaveFileIndex = do
  files <- liftIO $ getDirectoryContents "."

  let parseIndex file
        | saveFilePrefix `isPrefixOf` file
        , '-':index <- drop (length saveFilePrefix) file
        , [(x, "")] <- reads index -- safe version of read
        = Just x

        | otherwise
        = Nothing

  pure $ case mapMaybe parseIndex files of
    [] -> Nothing
    xs -> Just $ maximum xs

-- | Find the name for the current _active_ dhall save file
currentSaveFile :: MonadIO m => m (Maybe FilePath)
currentSaveFile =
  (fmap . fmap) (\i -> saveFilePrefix <> "-" <> show i) currentSaveFileIndex

-- | Find the name for the next dhall save file
nextSaveFile :: MonadIO m => m FilePath
nextSaveFile = do
  mIndex <- currentSaveFileIndex

  let nextIndex = maybe 0 succ mIndex

  pure $ saveFilePrefix <> "-" <> show nextIndex

loadBinding
  :: ( MonadFail m, MonadIO m, MonadState Env m, Haskeline.MonadException m )
  => [String] -> m ()
loadBinding [] = do
  mFile <- currentSaveFile

  case mFile of
    Just file -> loadBinding [file]
    Nothing   ->
      Fail.fail $ ":load couldn't find any `" <> saveFilePrefix <> "-*` files"

loadBinding [file] = do
  -- Read commands from the save file
  replLines <- map words . lines <$> liftIO (readFile file)

  let runCommand ((c:cmd):opts)
        | c == optionsPrefix
        , Just action <- lookup cmd options
        = action opts
      runCommand _ = Fail.fail $
        ":load expects `" <> file <> "` to contain one command per line"

  -- Keep current handle in scope
  Env { outputHandle } <- get

  -- Discard output
  modify (\e -> e { outputHandle = Nothing })

  -- Run all the commands
  forM_ replLines runCommand

  -- Restore the previous handle
  modify (\e -> e { outputHandle = outputHandle })

  writeOutputHandle $ "Loaded `" <> Text.pack file <> "`\n"

loadBinding _ = Fail.fail ":load should be of the form `:load` or `:load file`"

saveBinding :: ( MonadFail m, MonadIO m, MonadState Env m ) => [String] -> m ()
-- Save all the bindings into a context save file
saveBinding [] = do
  file <- nextSaveFile

  saveBinding [file]

-- Save all the bindings into `file`
saveBinding [file] = do
  env <- get

  let bindings
        = reverse
        . (fmap . fmap) bindingExpr
        . Dhall.Context.toList
        $ envBindings env

      handler handle =
          State.evalStateT
            (forM_ bindings $ \(name, expr) -> do
              liftIO (System.IO.hPutStr handle $ ":let " <> Text.unpack name <> " = ")
              outputWithoutSpacing expr)
            (env { outputHandle = Just handle })

  liftIO (System.IO.withFile file System.IO.WriteMode handler)

  writeOutputHandle $ "Context saved to `" <> Text.pack file <> "`\n"

-- Save a single expression to `file`
saveBinding (file : "=" : tokens) = do
  loadedExpression <- parseAndLoad (unwords tokens)

  _ <- typeCheck loadedExpression

  normalizedExpression <- normalize loadedExpression

  env <- get

  let handler handle =
          State.evalStateT
            (output normalizedExpression)
            (env { outputHandle = Just handle })

  liftIO (System.IO.withFile file System.IO.WriteMode handler)

  writeOutputHandle $ "Expression saved to `" <> Text.pack file <> "`\n"

saveBinding _ = Fail.fail ":save should be of the form `:save`, `:save file`, or `:save file = expr`"

setOption :: ( MonadIO m, MonadState Env m ) => [String] -> m ()
setOption [ "--explain" ] = do
  modify (\e -> e { explain = True })
setOption _ = do
  writeOutputHandle ":set should be of the form `:set <command line option>`"

unsetOption :: ( MonadIO m, MonadState Env m ) => [String] -> m ()
unsetOption [ "--explain" ] = do
  modify (\e -> e { explain = False })
unsetOption _ = do
  writeOutputHandle ":unset should be of the form `:unset <command line option>`"

cmdQuit :: ( MonadIO m, MonadState Env m ) => [String] -> m ()
cmdQuit _ = do
  liftIO (putStrLn "Goodbye.")
  liftIO (throwIO Interrupt)

help
  :: ( Haskeline.MonadException m, MonadFail m, MonadIO m, MonadState Env m )
  => HelpOptions m -> [String] -> m ()
help hs _ = do
  liftIO (putStrLn "Type any expression to normalize it or use one of the following commands:")
  forM_ hs $ \h -> do
    let name = helpOptionName h
        syntax = helpOptionSyntax h
        doc = helpOptionDoc h
    liftIO (putStrLn (":" <> name <> " " <> syntax))
    liftIO (putStrLn ("    " <> doc))

optionsPrefix :: Char
optionsPrefix = ':'

data HelpOption m = HelpOption
  { helpOptionName :: String
  , helpOptionSyntax :: String
  , helpOptionDoc :: String
  , helpOptionFunction :: Repline.Cmd m
  }

type HelpOptions m = [HelpOption m]

helpOptions
  :: ( Haskeline.MonadException m, MonadFail m, MonadIO m, MonadState Env m )
  => HelpOptions m
helpOptions =
  [ HelpOption
      "help"
      ""
      "Print help text and describe options"
      (dontCrash . help helpOptions)
  , HelpOption
      "type"
      "EXPRESSION"
      "Infer the type of an expression"
      (dontCrash . typeOf)
  , HelpOption
      "hash"
      "EXPRESSION"
      "Hash the normalized value of an expression"
      (dontCrash . hashBinding)
  , HelpOption
      "let"
      "IDENTIFIER = EXPRESSION"
      "Assign an expression to a variable"
      (dontCrash . addBinding . separateEqual)
  , HelpOption
      "clear"
      ""
      "Clear all bound variables"
      (dontCrash . clearBindings)
  , HelpOption
      "load"
      "[FILENAME]"
      "Load bound variables from a file"
      (dontCrash . loadBinding)
  , HelpOption
      "save"
      "[FILENAME | FILENAME = EXPRESSION]"
      "Save bound variables or a given expression to a file"
      (dontCrash . saveBinding . separateEqual)
  , HelpOption
      "set"
      "OPTION"
      "Set an option. Currently supported: --explain"
      (dontCrash . setOption)
  , HelpOption
      "unset"
      "OPTION"
      "Unset an option"
      (dontCrash . unsetOption)
  , HelpOption
      "quit"
      ""
      "Exit the REPL"
      cmdQuit
  ]

options
  :: ( Haskeline.MonadException m, MonadFail m, MonadIO m, MonadState Env m )
  => Repline.Options m
options = (\h -> (helpOptionName h, helpOptionFunction h)) <$> helpOptions

completer
  :: (Monad m, MonadFail m, MonadIO m, MonadState Env m)
  => Repline.CompleterStyle m
completer =
  Repline.Prefix
    (Haskeline.completeWordWithPrev (Just '\\') separators completeFunc)
    []
  where
    -- Separators that can be found on the left of something we want to
    -- autocomplete
    separators :: String
    separators = " \t[(,=+*&|}#?>:"

completeFunc
  :: (Monad m, MonadFail m, MonadIO m, MonadState Env m)
  => String -> String -> m [Completion]
completeFunc reversedPrev word

  -- Complete commands
  | reversedPrev == ":"
  = pure . listCompletion $ fst <$> (options :: Repline.Options Repl)

  -- Complete load command
  | reversedPrev == reverse ":load "
  = Haskeline.listFiles word

  -- Complete file paths
  | any (`isPrefixOf` word) [ "/", "./", "../", "~/" ]
  = Haskeline.listFiles word

  -- Complete environment variables
  | reverse "env:" `isPrefixOf` reversedPrev
  = listCompletion . fmap fst <$> liftIO getEnvironment

  -- Complete record fields and union alternatives
  | var : subFields <- Text.split (== '.') (Text.pack word)
  , not $ null subFields
  = do
    Env { envBindings } <- get

    case Dhall.Context.lookup var 0 envBindings of

      Nothing -> pure []

      Just binding -> do
        let candidates = algebraicComplete subFields (bindingExpr binding)
        pure $ listCompletion (Text.unpack . (var <>) <$> candidates)

  -- Complete variables in scope and all reserved identifiers
  | otherwise
  = do
    Env { envBindings } <- get

    let vars     = map fst $ Dhall.Context.toList envBindings
        reserved = Data.HashSet.toList Dhall.Core.reservedIdentifiers

    pure . listCompletion . map Text.unpack . nub $ vars ++ reserved

  where
    listCompletion = map simpleCompletion . filter (word `isPrefixOf`)

    algebraicComplete
        :: [Text.Text] -> Dhall.Expr Dhall.Src Void -> [Text.Text]
    algebraicComplete subFields expr =
      let keys = fmap ("." <>) . Map.keys

          withMap m =
              case subFields of
                  [] -> keys m
                  -- Stop on last subField (we care about the keys at this level)
                  [_] -> keys m
                  f:fs ->
                      case Map.lookup f m of
                          Nothing ->
                              []
                          Just Nothing ->
                              keys m
                          Just (Just e) ->
                              fmap (("." <> f) <>) (algebraicComplete fs e)

      in  case expr of
            Dhall.Core.RecordLit    m -> withMap (fmap Just m)
            Dhall.Core.Union        m -> withMap m
            _                         -> []


greeter :: MonadIO m => m ()
greeter =
  let version = Meta.dhallVersionString
      message = "Welcome to the Dhall v" <> version <> " REPL! Type :help for more information."
  in liftIO (putStrLn message)


dontCrash :: ( MonadIO m, Haskeline.MonadException m ) => m () -> m ()
dontCrash m =
  Haskeline.catch
    m
    ( \ e@SomeException{} -> liftIO ( putStrLn ( displayException e ) ) )

writeOutputHandle :: (MonadIO m, MonadState Env m) => Text -> m ()
writeOutputHandle txt = do
  Env { outputHandle } <- get

  case outputHandle of
    Just handle -> liftIO $ Text.IO.hPutStrLn handle txt
    Nothing     -> pure ()

output
  :: (Pretty.Pretty a, MonadState Env m, MonadIO m)
  => Dhall.Expr Src a -> m ()
output expr = do
  writeOutputHandle "" -- Visual spacing

  outputWithoutSpacing expr

  writeOutputHandle "" -- Visual spacing

outputWithoutSpacing
  :: (Pretty.Pretty a, MonadState Env m, MonadIO m)
  => Dhall.Expr Src a -> m ()
outputWithoutSpacing expr = do
  Env { characterSet, outputHandle } <- get

  case outputHandle of
    Nothing     -> pure ()
    Just handle -> do
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
