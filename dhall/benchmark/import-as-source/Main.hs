{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception (bracket, throw)
import Data.Text         (Text)
import Data.Void         (Void)
import System.FilePath   ((</>), takeDirectory)
import Test.Tasty.Bench

import qualified Data.Text.IO       as Text
import qualified Dhall
import qualified Dhall.Core         as Core
import qualified Dhall.Parser       as Parser
import qualified Dhall.TypeCheck    as TypeCheck
import qualified Lens.Micro         as Lens
import qualified System.Directory   as Directory
import qualified System.Environment as Environment
import qualified System.IO.Temp     as Temp

type ResolvedExpr = Core.Expr Parser.Src Void

data Fixture = Fixture
    { fixtureName    :: String
    , codePath       :: FilePath
    , codeText       :: Text
    , sourcePath     :: FilePath
    , sourceText     :: Text
    , resolvedCode   :: ResolvedExpr
    , resolvedSource :: ResolvedExpr
    }

main :: IO ()
main = do
    fixturesDirectory <- Directory.makeAbsolute "benchmark/import-as-source/fixtures"
    fixtures <- traverse (loadFixture fixturesDirectory) fixtureNames

    defaultMain
        [ bgroup "import-as-source"
            (map benchFixture fixtures)
        ]

fixtureNames :: [String]
fixtureNames =
    [ "assert"
    , "field"
    , "typecheck"
    ]

loadFixture :: FilePath -> String -> IO Fixture
loadFixture fixturesDirectory name = do
    let importersDirectory = fixturesDirectory </> "importers"

    let codePath = importersDirectory </> (name ++ "-code.dhall")
    let sourcePath = importersDirectory </> (name ++ "-source.dhall")

    codeText <- Text.readFile codePath
    sourceText <- Text.readFile sourcePath

    resolvedCode <- resolveCold codePath codeText
    resolvedSource <- resolveCold sourcePath sourceText

    pure
        Fixture
            { fixtureName = name
            , codePath = codePath
            , codeText = codeText
            , sourcePath = sourcePath
            , sourceText = sourceText
            , resolvedCode = resolvedCode
            , resolvedSource = resolvedSource
            }

benchFixture :: Fixture -> Benchmark
benchFixture Fixture{..} =
    bgroup fixtureName
        [ bgroup "resolve-cold"
            [ bench "code" $ nfIO (resolveCold codePath codeText)
            , bench "source" $ nfIO (resolveCold sourcePath sourceText)
            ]
        , bgroup "typecheck-resolved"
            [ bench "code" $ nf typecheckResolvedExpr resolvedCode
            , bench "source" $ nf typecheckResolvedExpr resolvedSource
            ]
        ]

resolveCold :: FilePath -> Text -> IO ResolvedExpr
resolveCold path text = withFreshCacheHome $ do
    parsed <- either throw pure (Parser.exprFromText path text)

    let settings =
            Lens.set Dhall.sourceName path
                (Lens.set Dhall.rootDirectory (takeDirectory path) Dhall.defaultInputSettings)

    Dhall.resolveWithSettings settings parsed

withFreshCacheHome :: IO a -> IO a
withFreshCacheHome action = do
    originalCacheHome <- Environment.lookupEnv "XDG_CACHE_HOME"

    Temp.withSystemTempDirectory "dhall-import-as-source-bench" $ \cacheHome ->
        bracket
            (Environment.setEnv "XDG_CACHE_HOME" cacheHome)
            (\() -> restoreCacheHome originalCacheHome)
            (\() -> action)

restoreCacheHome :: Maybe String -> IO ()
restoreCacheHome = maybe (Environment.unsetEnv "XDG_CACHE_HOME") (Environment.setEnv "XDG_CACHE_HOME")

typecheckResolvedExpr :: ResolvedExpr -> Maybe (Core.Expr Parser.Src Void)
typecheckResolvedExpr = either (const Nothing) Just . TypeCheck.typeOf
