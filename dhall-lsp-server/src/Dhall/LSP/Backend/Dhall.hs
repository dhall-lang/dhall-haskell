module Dhall.LSP.Backend.Dhall where

import Dhall.Import (loadWith, emptyStatus)
import Dhall.Parser (Src, exprFromText)
import Dhall.TypeCheck (X)
import Dhall.Core (Expr)
import Dhall
  (rootDirectory, sourceName, defaultInputSettings, inputExprWithSettings)

import Data.Text (Text)
import System.FilePath (splitFileName)
import Lens.Family (set)
import Control.Exception (handle, SomeException)
import Control.Monad.Trans.State.Strict (evalStateT)

runDhall :: FilePath -> Text -> IO (Expr Src X)
runDhall path = inputExprWithSettings dhallparams
  where
    dhallparams = (set rootDirectory dir . set sourceName file)
                  defaultInputSettings
    (dir, file) = splitFileName path

runDhallSafe :: FilePath -> Text -> IO (Maybe (Expr Src X))
runDhallSafe path text = handle (\(_ :: SomeException) -> return Nothing)
                                (Just <$> runDhall path text)

loadDhallExprSafe :: FilePath -> Text -> IO (Maybe (Expr Src X))
loadDhallExprSafe filePath txt =
  case exprFromText filePath txt of
    Right expr ->
      let (dir, _) = splitFileName filePath
      in handle (\(_ :: SomeException) -> return Nothing)
                (Just <$> evalStateT (loadWith expr) (emptyStatus dir))
    Left _ -> return Nothing
