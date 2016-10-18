module Main where

import Control.Exception (Exception, throwIO)
import Data.Monoid (mempty)
import Data.Traversable
import Dhall.Core (pretty, normalize)
import Dhall.Import (load)
import Dhall.Parser (exprFromBytes)
import Options.Applicative hiding (Const)
import System.IO (stderr)
import System.Exit (exitFailure)

import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy.IO
import qualified Dhall.TypeCheck

throws :: Exception e => Either e a -> IO a
throws (Left  e) = throwIO e
throws (Right r) = return r

data Mode = Default | Resolve | TypeCheck | Normalize

parser :: Parser Mode
parser
    =   subparser
        (   command "resolve"
            (info (helper <*> pure Resolve)
                (   fullDesc
                <>  header "dhall resolve - Resolve Dhall code"
                <>  progDesc "Transitively replace all remote paths and URLs \
                             \with the code that they refer to, reading the \
                             \program from standard input and writing the \
                             \fully resolved program to standard output."
                )
            )
        <>  metavar "resolve"
        )
    <|> subparser
        (   command "typecheck"
            (   info (helper <*> pure TypeCheck)
                (   fullDesc
                <>  header "dhall typecheck - Type-check Dhall code"
                <>  progDesc "Verify that Dhall code is well-formed by \
                             \type-checking the program, reading the program \
                             \from standard input and writing the program's \
                             \inferred type to standard output."
                ) )
        <>  metavar "typecheck" )
    <|> subparser
        (   command "normalize"
            (   info (helper <*> pure Normalize)
                (   fullDesc
                <>  header "dhall normalize - Normalize Dhall code"
                <>  progDesc "Reduce Dhall code to normal form using \
                             \β-reduction and η-reduction, reading the program \
                             \from standard input and writing the normalized \
                             \program to standard output."
                ) )
        <>  metavar "normalize"
        )
    <|> pure Default

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> parser) 
        (   fullDesc
        <>  header "dhall - A bare-bones calculus of constructions"
        <>  progDesc "Type-check, resolve, and normalize a Dhall program, \
                     \reading the program from standard input, writing the \
                     \program's normalized type to standard error, and writing \
                     \the normalized program to standard output."
        )
    case mode of
        Default -> do
            inBytes  <- Data.ByteString.Lazy.getContents
            expr     <- throws (exprFromBytes inBytes)
            expr'    <- load Nothing expr
            typeExpr <- throws (Dhall.TypeCheck.typeOf expr')
            Data.Text.Lazy.IO.hPutStrLn stderr (pretty (normalize typeExpr))
            Data.Text.Lazy.IO.hPutStrLn stderr mempty
            Data.Text.Lazy.IO.putStrLn (pretty (normalize expr'))
        Resolve   -> do
            inBytes <- Data.ByteString.Lazy.getContents
            expr    <- throws (exprFromBytes inBytes)
            expr'   <- load Nothing expr
            Data.Text.Lazy.IO.putStrLn (pretty expr')
        TypeCheck -> do
            inBytes <- Data.ByteString.Lazy.getContents
            expr   <- throws (exprFromBytes inBytes)
            case traverse (\_ -> Nothing) expr of
                Nothing    -> throwIO (userError
                    "`dhall typecheck` cannot type-check a program containing \
                    \remote references.  Use `dhall resolve` to resolve all \
                    \remote references or just use `dhall` which combines \
                    \resolution, type-checking, and normalization." )
                Just expr' -> do
                    typeExpr <- throws (Dhall.TypeCheck.typeOf expr')
                    Data.Text.Lazy.IO.putStrLn (pretty typeExpr)
        Normalize -> do
            inBytes <- Data.ByteString.Lazy.getContents
            expr   <- throws (exprFromBytes inBytes)
            Data.Text.Lazy.IO.putStrLn (pretty (normalize expr))
