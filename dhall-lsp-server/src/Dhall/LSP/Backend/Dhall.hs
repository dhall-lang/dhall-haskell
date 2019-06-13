module Dhall.LSP.Backend.Dhall where

import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)
import Dhall.Core (Expr)
import Dhall
  (rootDirectory, sourceName, defaultInputSettings, inputExprWithSettings)

import Data.Text (Text)
import System.FilePath (splitFileName)
import Lens.Family (set)
import Control.Exception (handle, SomeException)

runDhall :: FilePath -> Text -> IO (Expr Src X)
runDhall path = inputExprWithSettings dhallparams
  where
    dhallparams = (set rootDirectory dir . set sourceName file)
                  defaultInputSettings
    (dir, file) = splitFileName path

runDhallSafe :: FilePath -> Text -> IO (Maybe (Expr Src X))
runDhallSafe path text = handle (\(_ :: SomeException) -> return Nothing)
                                (Just <$> runDhall path text)
