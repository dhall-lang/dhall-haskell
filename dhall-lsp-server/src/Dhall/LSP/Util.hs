-- | Miscellaneous utility functions

module Dhall.LSP.Util (
  tshow,
  lines',
  readUri,
  rightToMaybe,
  unlines'
) where

import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.VFS as LSP
import qualified Language.Haskell.LSP.Types as J
import qualified Yi.Rope as Rope

import Data.Text
import Data.List.NonEmpty

-- | Shorthand for @pack . show@. Useful since we are mostly working with Text
--   rather than String.
tshow :: Show a => a -> Text
tshow = pack . show

-- | A variant of @Data.Text.lines@ that does not swallow the last empty. Always
--   returns at least the empty line!
lines' :: Text -> NonEmpty Text
lines' text =
    case split (== '\n') text of
      []     -> "" :| []  -- this case never occurs!
      l : ls -> l  :| ls

-- | A variant of @Data.Text.unlines@ that is the exact inverse to @lines'@ (and
--   vice-versa).
unlines' :: [Text] -> Text
unlines' = intercalate "\n"

-- | A helper function to query haskell-lsp's VFS.
readUri :: LSP.LspFuncs () -> J.Uri -> IO Text
readUri lsp uri = do
  asd <- LSP.getVirtualFileFunc lsp uri
  case asd of
    Just (LSP.VirtualFile _ rope) -> return (Rope.toText rope)
    Nothing -> fail $ "Could not find " <> show uri <> " in VFS."

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right b) = Just b
rightToMaybe (Left _) = Nothing
