{-| Common utilities for @dhall-docs@ -}
module Dhall.Docs.Util where

import Data.Text (Text)

import qualified Data.Text

-- | If you're wondering the GitHub query params for issue creation:
-- https://docs.github.com/en/github/managing-your-work-on-github/about-automation-for-issues-and-pull-requests-with-query-parameters
fileAnIssue :: Text -> a
fileAnIssue titleName =
    error $ "\ESC[1;31mError\ESC[0m Documentation generator bug\n\n" <>

            "Explanation: This error message means that there is a bug in the " <>
            "Dhall Documentation generator. You didn't do anything wrong, but " <>
            "if you would like to see this problem fixed then you should report " <>
            "the bug at:\n\n" <>

            "https://github.com/dhall-lang/dhall-haskell/issues/new?labels=dhall-docs,bug\n\n" <>

            "explaining your issue and add \"" <> Data.Text.unpack titleName <> "\" as error code " <>
            "so we can find the proper location in the source code where the error happened\n\n" <>

            "Please, also include your package in the issue. It can be in:\n\n" <>
            "* A compressed archive (zip, tar, etc)\n" <>
            "* A git repository, preferably with a commit reference"


