module Dhall.LSP.Backend.Prefix
    ( resumeTypingContext
    , topLevelPrefixLength
    , topLets
    ) where

import Data.Text (Text)
import Data.Void (Void)

import Dhall.Core
    ( Binding (..)
    , Expr (..)
    , denote
    )
import Dhall.Parser (Src)
import Dhall.TypeCheck (TypingContext, emptyTypingContext)

import qualified Dhall.Core as Core

-- | Top-level @let@ bindings and the remaining expression.
topLets
    :: Expr Src a
    -> ([(Text, Maybe (Maybe Src, Expr Src a), Expr Src a)], Expr Src a)
topLets (Note _ expr) = topLets expr
topLets (Core.Let Core.Binding { Core.variable = name, Core.annotation = ann, Core.value = value } expr) =
    let (binds, rest) = topLets expr
    in ((name, ann, value) : binds, rest)
topLets expr = ([], expr)

-- | How many leading top-level bindings match the previous analysis snapshot.
topLevelPrefixLength
    :: [Text]
    -> [Core.Expr Void Void]
    -> [(Text, Maybe (Maybe Src, Expr Src Void), Expr Src Void)]
    -> Int
topLevelPrefixLength prevNames prevValues binds =
    length
        [ ()
        | (i, (name, _, value)) <- zip [0 ..] binds
        , i < length prevNames
        , i < length prevValues
        , name == prevNames !! i
        , (denote value :: Core.Expr Void Void) == prevValues !! i
        ]

-- | Typing context after the longest unchanged top-level @let@ prefix.
resumeTypingContext
    :: [Text]
    -> [Core.Expr Void Void]
    -> [TypingContext Src]
    -> Expr Src Void
    -> TypingContext Src
resumeTypingContext prevNames prevValues prevCtxs expr =
    let (binds, _) = topLets expr
        k = topLevelPrefixLength prevNames prevValues binds
    in case k of
        0 -> emptyTypingContext
        n -> prevCtxs !! (n - 1)
