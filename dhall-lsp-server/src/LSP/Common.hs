{-| Common utilities / types for the LSP part -}
module LSP.Common(sendToClient, nextLspReqId) where

import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Core as LSP.Core

import qualified Data.Aeson                            as J
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J
import Control.Monad.Reader.Class (ask, asks)
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (liftIO)


sendToClient :: FromServerMessage -> ReaderT (LSP.Core.LspFuncs ()) IO ()
sendToClient msg = do
  lf <- ask
  liftIO $ LSP.Core.sendFunc lf msg

nextLspReqId :: ReaderT (LSP.Core.LspFuncs ()) IO J.LspId
nextLspReqId = asks LSP.Core.getNextReqId >>= liftIO