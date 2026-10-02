module Dhall.LSP.Backend.Prefix
    ( bindingValueKeys
    , resumeTypingContext
    , topLevelPrefixLength
    , topLets
    , valueSourceKey
    ) where

import Data.Text (Text)
import Data.Void (Void)

import Dhall.Core
    ( Binding (..)
    , Expr (..)
    , denote
    )
import Dhall.Parser (Src (..))
import Dhall.TypeCheck (TypingContext, emptyTypingContext)

import Dhall.LSP.Backend.Diagnostics
    ( Range (..)
    , positionToOffset
    , rangeFromDhall
    )

import qualified Dhall.Core as Core
import qualified Data.Text as Text

-- | Top-level @let@ bindings and the remaining expression.
topLets
    :: Expr Src a
    -> ([(Text, Maybe (Maybe Src, Expr Src a), Expr Src a)], Expr Src a)
topLets (Note _ expr) = topLets expr
topLets (Core.Let Core.Binding { Core.variable = name, Core.annotation = ann, Core.value = value } expr) =
    let (binds, rest) = topLets expr
    in ((name, ann, value) : binds, rest)
topLets expr = ([], expr)

-- | Source text of a binding value from the file buffer (when annotated).
valueSourceKey :: Text -> Expr Src a -> Text
valueSourceKey file expr =
    case expr of
        Note src _ ->
            let Range left right = rangeFromDhall src
                from = positionToOffset file left
                to = positionToOffset file right
            in Text.take (max 0 (to - from)) (Text.drop from file)
        _ ->
            ""

-- | Keys for each top-level binding in a parsed file.
bindingValueKeys :: Text -> Expr Src a -> [Text]
bindingValueKeys file expr =
    [ valueSourceKey file value | (_, _, value) <- fst (topLets expr) ]

-- | How many leading top-level bindings match the previous analysis snapshot.
topLevelPrefixLength
    :: [Text]
    -> [Text]
    -> [Text]
    -> [Core.Expr Void Void]
    -> [(Text, Maybe (Maybe Src, Expr Src Void), Expr Src Void)]
    -> Int
topLevelPrefixLength prevNames prevKeys currentKeys prevValues binds =
    if useKeys prevNames prevKeys currentKeys
        then
            length
                [ ()
                | (i, ((name, _, _), key)) <- zip [0 ..] (zip binds currentKeys)
                , i < length prevNames
                , i < length prevKeys
                , name == prevNames !! i
                , key == prevKeys !! i
                ]
        else
            length
                [ ()
                | (i, (name, _, value)) <- zip [0 ..] binds
                , i < length prevNames
                , i < length prevValues
                , name == prevNames !! i
                , (denote value :: Core.Expr Void Void) == prevValues !! i
                ]

useKeys :: [Text] -> [Text] -> [Text] -> Bool
useKeys prevNames prevKeys currentKeys =
    not (null prevKeys)
        && length prevKeys == length prevNames
        && length currentKeys == length prevNames

-- | Typing context after the longest unchanged top-level @let@ prefix.
resumeTypingContext
    :: [Text]
    -> [Text]
    -> [Text]
    -> [Core.Expr Void Void]
    -> [TypingContext Src]
    -> Expr Src Void
    -> TypingContext Src
resumeTypingContext prevNames prevKeys currentKeys prevValues prevCtxs expr =
    let (binds, _) = topLets expr
        k = topLevelPrefixLength prevNames prevKeys currentKeys prevValues binds
    in case k of
        0 -> emptyTypingContext
        n -> prevCtxs !! (n - 1)
