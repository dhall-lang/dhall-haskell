module Dhall.LSP.Handlers.DocumentFormatting(formatDocument) where



import qualified Dhall.LSP.Backend.Formatting as Formatting

import qualified Language.Haskell.LSP.Core as LSP.Core

import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Utility  as LSP.Utility

import qualified Data.Text
import qualified Data.Text.IO
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT)

-- TODO: implement tabSize and spaces/tabs options
-- * Note: any formatting errors would be swallowed. I think this is fine in this case, but generally we'd like to send user a notification
-- (e.g. the error occurred in the formatter itself, and user requests format constantly and nothing happens)
formatDocument :: J.Uri -> Int -> Bool -> ReaderT (LSP.Core.LspFuncs ()) IO (J.List J.TextEdit)
formatDocument fileUri _tabSize _insertSpaces = do
    let
      filePath = maybe (error "can't convert uri to file path") id $ J.uriToFilePath fileUri -- !FIXME: handle non-file uris
    txt <- lift $ Data.Text.IO.readFile filePath
    case Formatting.formatDocument txt of 
      (Right formatted) -> let
                             numLines = Data.Text.length txt
                             range = J.Range  (J.Position 0 0) (J.Position numLines 0)
                           in pure $ J.List [J.TextEdit range formatted]
      (Left err) -> do
                  lift $ LSP.Utility.logs $ "Error while formatting the document " <> show err
                  pure (J.List [])
    
