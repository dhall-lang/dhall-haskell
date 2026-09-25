-- | Internal line editor for @dhall repl@.
--
-- This covers the subset of @repline-0.4.2.0@ that 'Dhall.Repl' used: the
-- 'HaskelineT' wrapper, @:@ command dispatch, @:paste@ input, tab completion,
-- and the @.history@ file. Behavior of that subset is unchanged.
--
-- The read loop is adapted from repline, which is MIT licensed:
--
-- Copyright (c) 2016-2020 Stephen Diehl
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module Dhall.Repl.Line
    ( -- * Repl
      HaskelineT
    , evalRepl
    , Cmd
    , Options
    , CompleterStyle(..)
    , ExitDecision(..)
    , MultiLine(..)
    ) where

import Control.Monad.Catch                 (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class              (MonadIO)
import Control.Monad.State.Class           (MonadState (..))
import Control.Monad.Trans.Class           (lift)
import Data.List                           (isPrefixOf)
import System.Console.Haskeline.Completion (CompletionFunc)

import qualified Control.Monad.Fail         as Fail
import qualified System.Console.Haskeline   as Haskeline

-- | Monad transformer for line input.
--
-- Lifts 'MonadState' and 'MonadFail' through Haskeline's @InputT@, which is
-- the reason 'Dhall.Repl' used @repline@'s wrapper rather than @InputT@
-- directly.
newtype HaskelineT m a = HaskelineT (Haskeline.InputT m a)
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

instance Fail.MonadFail m => Fail.MonadFail (HaskelineT m) where
    fail = HaskelineT . lift . Fail.fail

instance MonadState s m => MonadState s (HaskelineT m) where
    get = HaskelineT (lift get)
    put s = HaskelineT (lift (put s))

-- | Command function synonym.
--
-- The argument is the text after the command name. It may contain spaces or
-- newlines (when input was collected with the multi-line command).
--
-- With prefix @':'@ and command @"command"@, the argument for
--
-- > :command some arguments
--
-- is @"some arguments"@.
type Cmd m = String -> m ()

-- | Association list of command names and the actions they run.
type Options m = [(String, Cmd m)]

-- | Evaluation function for a line that is not a @:@ command.
type Command m = String -> m ()

-- | Whether to close the REPL after Ctrl-D.
data ExitDecision
    = Continue
    -- ^ Keep the REPL open
    | Exit
    -- ^ Close the REPL and exit
    deriving (Eq, Show)

-- | Whether the current prompt is collecting a multi-line command.
data MultiLine
    = SingleLine
    | MultiLine
    deriving (Eq, Show)

-- | Tab-completion style.
--
-- 'Prefix' tries each association in order. The command name must be a prefix
-- of the text to the left of the cursor. The 'CompletionFunc' is used when
-- none of those names match. An empty association list uses the default
-- completer unchanged.
data CompleterStyle m
    = Prefix (CompletionFunc m) [(String, CompletionFunc m)]

-- | Prompt, read a line, and either dispatch a command or run the evaluator.
--
-- Ctrl-D on the main prompt runs the finaliser. Ctrl-C on an empty prompt
-- clears the line and continues. Ctrl-C during a command or an evaluation
-- exits the loop without running the finaliser, which is how @:quit@ exits
-- after throwing Haskeline's 'Haskeline.Interrupt'.
--
-- History is appended to @.history@ in the current directory.
evalRepl
    :: (MonadMask m, MonadIO m)
    => (MultiLine -> HaskelineT m String)
    -- ^ Banner. Receives 'SingleLine' or 'MultiLine'.
    -> Command (HaskelineT m)
    -- ^ Evaluator for a line that is not a command.
    -> Options (HaskelineT m)
    -- ^ Commands selected by the prefix character.
    -> Maybe Char
    -- ^ Command prefix, such as @':'@. 'Nothing' disables commands.
    -> Maybe String
    -- ^ Command name that switches to multi-line input until Ctrl-D.
    -- 'Nothing' disables that mode.
    -> CompleterStyle m
    -- ^ Tab completion. Runs in the base monad, not in 'HaskelineT'.
    -> HaskelineT m a
    -- ^ Initialiser, run once before the loop.
    -> HaskelineT m ExitDecision
    -- ^ Finaliser, run on Ctrl-D from the main prompt.
    -> m ()
evalRepl banner command options prefix multilineCommand completer initialiser finaliser =
    runHaskelineT settings (initialiser >> loop)
  where
    settings =
        Haskeline.Settings
            { Haskeline.complete = mkCompleter completer
            , Haskeline.historyFile = Just ".history"
            , Haskeline.autoAddHistory = True
            }

    runHaskelineT s (HaskelineT input) =
        Haskeline.runInputT s (Haskeline.withInterrupt input)

    loop = replLoop banner command options prefix multilineCommand finaliser

-- | Read lines until the finaliser asks to exit.
replLoop
    :: (MonadMask m, MonadIO m)
    => (MultiLine -> HaskelineT m String)
    -> Command (HaskelineT m)
    -> Options (HaskelineT m)
    -> Maybe Char
    -> Maybe String
    -> HaskelineT m ExitDecision
    -> HaskelineT m ()
replLoop banner command options prefix multilineCommand finaliser = loop
  where
    loop = do
        prompt <- banner SingleLine
        minput <-
            Haskeline.handleInterrupt (return (Just "")) (getInputLine prompt)
        handleCommands minput

    handleCommands minput =
        case minput of
            Nothing ->
                finaliser >>= \decision ->
                    case decision of
                        Continue -> loop
                        Exit -> return ()
            Just "" ->
                loop
            Just (first : rest)
                | null rest ->
                    handleInput [first] >> loop
                | Just first == prefix ->
                    case words rest of
                        [] ->
                            loop
                        (name : _)
                            | Just name == multilineCommand -> do
                                outputStrLn
                                    "-- Entering multi-line mode. Press <Ctrl-D> to finish."
                                loopMultiLine []
                        (name : _) -> do
                            -- When arguments are present, the name is followed
                            -- by a whitespace character, so drop one more than
                            -- the length of the name.
                            let arguments = drop (1 + length name) rest
                            result <-
                                Haskeline.handleInterrupt (return Nothing) $
                                    Just <$> optMatcher name options arguments
                            case result of
                                Nothing -> return ()
                                Just () -> loop
            Just input -> do
                handleInput input
                loop

    loopMultiLine prevs = do
        prompt <- banner MultiLine
        minput <-
            Haskeline.handleInterrupt (return (Just "")) (getInputLine prompt)
        case minput of
            Nothing ->
                handleCommands (Just (unlines (reverse prevs)))
            Just line ->
                loopMultiLine (line : prevs)

    handleInput input =
        Haskeline.handleInterrupt (return ()) (command input)

-- | Run the first command whose name has @name@ as a prefix.
optMatcher
    :: MonadIO m
    => String
    -> Options (HaskelineT m)
    -> String
    -> HaskelineT m ()
optMatcher name [] _ =
    outputStrLn ("No such command :" ++ name)
optMatcher name ((candidate, action) : rest) arguments
    | name `isPrefixOf` candidate = action arguments
    | otherwise = optMatcher name rest arguments

getInputLine
    :: (MonadIO m, MonadMask m) => String -> HaskelineT m (Maybe String)
getInputLine = HaskelineT . Haskeline.getInputLine

outputStrLn :: MonadIO m => String -> HaskelineT m ()
outputStrLn = HaskelineT . Haskeline.outputStrLn

mkCompleter :: Monad m => CompleterStyle m -> CompletionFunc m
mkCompleter (Prefix fallback named) = runMatcher named fallback

-- | Pick a completer from the line so far.
--
-- Haskeline passes the text to the left of the cursor reversed. The matcher
-- puts it back into display order before comparing prefixes.
runMatcher
    :: Monad m
    => [(String, CompletionFunc m)]
    -> CompletionFunc m
    -> CompletionFunc m
runMatcher named fallback (reversedLeft, word) =
    completeMatcher fallback (word ++ reverse reversedLeft) named (reversedLeft, word)

completeMatcher
    :: Monad m
    => CompletionFunc m
    -> String
    -> [(String, CompletionFunc m)]
    -> CompletionFunc m
completeMatcher fallback _ [] args = fallback args
completeMatcher fallback [] _ args = fallback args
completeMatcher fallback line ((name, completer) : rest) args
    | name `isPrefixOf` line = completer args
    | otherwise = completeMatcher fallback line rest args
