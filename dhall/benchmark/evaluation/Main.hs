module Main where

import Control.Exception (throw)
import Data.List         (isSuffixOf, sort)
import Data.Void         (Void)
import System.FilePath   ((</>), takeBaseName, takeDirectory)
import Test.Tasty.Bench

import qualified Data.Text.IO      as Text
import qualified Dhall
import qualified Dhall.Core        as Core
import qualified Dhall.Parser      as Parser
import qualified Dhall.TypeCheck   as TypeCheck
import qualified Lens.Micro
import qualified System.Directory  as Directory

type ResolvedExpr = Core.Expr Parser.Src Void

loadExamples :: IO [(String, ResolvedExpr)]
loadExamples = do
    files <- sort <$> Directory.listDirectory normalizeDirectory
    traverse loadExample
        [ normalizeDirectory </> file | file <- files, ".dhall" `isSuffixOf` file ]
  where
    normalizeDirectory = "benchmark/evaluation/normalize"

loadExample :: FilePath -> IO (String, ResolvedExpr)
loadExample path = do
    text <- Text.readFile path

    parsed <-
        either throw pure (Parser.exprFromText path text)

    let settings =
            Lens.Micro.set Dhall.sourceName path
                (Lens.Micro.set Dhall.rootDirectory (takeDirectory path) Dhall.defaultInputSettings)

    resolved <- Dhall.resolveWithSettings settings parsed

    pure (takeBaseName path, resolved)

main :: IO ()
main = do
    examples <- loadExamples

    defaultMain
        [ bgroup
            "normalize"
            [ bgroup
                name
                [ bench "typecheck" (nf typecheckResolvedExpr expression)
                , bench "evaluation" (nf normalizeResolvedExpr expression)
                ]
            | (name, expression) <- examples
            ]
        ]
 where
   -- These helpers are needed just to reduce polymorphism in TypeCheck.typeOf and Core.ormalize.
   typecheckResolvedExpr :: ResolvedExpr -> Maybe (Core.Expr Parser.Src Void)
   typecheckResolvedExpr = either (const Nothing) Just . TypeCheck.typeOf

   normalizeResolvedExpr :: ResolvedExpr -> ResolvedExpr
   normalizeResolvedExpr = Core.normalize
