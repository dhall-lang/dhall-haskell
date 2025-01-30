-- | Miscellaneous utility functions

module Dhall.LSP.Util (
  tshow,
  lines',
  rightToMaybe,
  unlines'
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

import qualified Data.Text as Text

-- | Shorthand for @pack . show@. Useful since we are mostly working with Text
--   rather than String.
tshow :: Show a => a -> Text
tshow = Text.pack . show

-- | A variant of @Data.Text.lines@ that does not swallow the last empty. Always
--   returns at least the empty line!
lines' :: Text -> NonEmpty Text
lines' text =
    case Text.split (== '\n') text of
      []     -> "" :| []  -- this case never occurs!
      l : ls -> l  :| ls

-- | A variant of @Data.Text.unlines@ that is the exact inverse to @lines'@ (and
--   vice-versa).
unlines' :: [Text] -> Text
unlines' = Text.intercalate "\n"

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right b) = Just b
rightToMaybe (Left _) = Nothing
