{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

-- | Create a package.dhall from files and directory contents.

module Dhall.Package
    ( writePackage
    , getPackageExpression
    ) where

import           Control.Monad
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Dhall.Core
    ( Directory (..)
    , Expr (..)
    , File (..)
    , FilePrefix (..)
    , Import (..)
    , ImportHashed (..)
    , ImportMode (..)
    , ImportType (..)
    , RecordField
    , makeRecordField
    )
import           Dhall.Map          (Map)
import qualified Dhall.Map          as Map
import           Dhall.Pretty       (CharacterSet (..))
import           Dhall.Util         (Input (..), Output (..), renderExpression)
import           System.Directory
import           System.FilePath

-- | Create a package.dhall from files and directory contents.
-- For a description of how the package.dhall is constructed see 'getPackageExpression'.
writePackage :: CharacterSet -> Bool -> Output -> NonEmpty Input -> IO ()
writePackage characterSet plain output inputs = do
    let output' = case output of
            StandardOutput -> Nothing
            OutputFile path -> Just path
    expr <- getPackageExpression output inputs
    renderExpression characterSet plain output' expr

-- | Get the Dhall expression for a package.dhall file.
--
-- If the 'Output' provided as the first argument is 'StandardOutput' the paths
-- of the imports are constructed as if the package.dhall would reside in the
-- same directory. That means, if you feed files from different directories as
-- 'Input' to this function the resulting expression may have unresolvable
-- imports!
--
-- If one of the 'Input' is 'StandardInput' then additional paths with be read
-- from stdin (newline separated). If a path provided as 'Input' is a directory
-- then all files with a ".dhall" extension will be included in the package. An
-- exception to this is the output file; It will be excluded from package.
getPackageExpression :: Output -> NonEmpty Input -> IO (Expr s Import)
getPackageExpression output inputs = do
    output' <- case output of
        StandardOutput -> pure Nothing
        OutputFile path -> Just . normaliseWithParent <$> makeAbsolute path
    RecordLit <$> getInputsMap output' (NonEmpty.toList inputs)

getInputsMap :: Maybe FilePath -> [Input] -> IO (Map Text (RecordField s Import))
getInputsMap output = foldMap (getInputMap output)

getInputMap :: Maybe FilePath -> Input -> IO (Map Text (RecordField s Import))
getInputMap output StandardInput = do
    inputs <- map InputFile . lines <$> getContents
    getInputsMap output inputs
getInputMap output (InputFile path) = do
    isDirectory <- doesDirectoryExist path
    if isDirectory
        then do
            entries <- listDirectory path
            paths <- traverse (fmap normaliseWithParent . makeAbsolute . (path </>)) entries
            include <- filterM (\entry -> do
                isFile <- doesFileExist entry
                let hasDhallExtension = takeExtension entry == ".dhall"
                let isNotOutputFile = Just entry /= output
                return $ isFile && hasDhallExtension && isNotOutputFile
                ) paths
            getInputsMap output $ map InputFile include
        else do
            path' <- normaliseWithParent <$> makeAbsolute path

            let key = Text.pack $ dropExtension $ takeFileName path

            let importType = getImportType output path'

            let import_ = Import
                    { importHashed = ImportHashed
                        { hash = Nothing
                        , ..
                        }
                    , importMode = Code
                    }
            return $ Map.singleton key (makeRecordField $ Embed import_)

getImportType :: Maybe FilePath -> FilePath -> ImportType
getImportType output path = let
    (prefix, base', path') = case output of
        Nothing -> (Here, [], [])
        Just output' -> goFilePrefix
            (splitDirectories $ takeDirectory output')
            (splitDirectories $ takeDirectory path)
    directory = goDirectory [] base' path'
    file = Text.pack $ takeFileName path
    in Local prefix File{..}
    where
        goFilePrefix [] yss = (Here, [], yss)
        goFilePrefix (_:xs) [] = (Parent, xs, [])
        goFilePrefix (x:xs) yss@(y:ys)
            | x == y = goFilePrefix xs ys
            | otherwise = (Parent, xs, yss)

        goDirectory !acc [] [] = Directory (map Text.pack acc)
        goDirectory !acc [] (y:ys) = goDirectory (y:acc) [] ys
        goDirectory !acc (_:xs) [] = goDirectory ("..":acc) xs []
        goDirectory !acc (x:xs) yss@(y:ys)
            | x == y = goDirectory acc xs ys
            | otherwise = goDirectory ("..":acc) xs yss

normaliseWithParent :: FilePath -> FilePath
normaliseWithParent = go [] . splitPath . normalise
    where
        go !acc [] = joinPath (reverse acc)
        go (_:acc) ("../":xs) = go acc xs
        go !acc (x:xs) = go (x:acc) xs
