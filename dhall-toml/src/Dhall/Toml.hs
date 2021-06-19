{-# LANGUAGE OverloadedStrings #-}

module Dhall.Toml
    ( tomlToDhallMain
    , tomlToDhall
    , dhallToTomlMain
    , dhallToToml
    ) where


import Control.Exception (Exception)
import Data.Void         (Void)
import Dhall.Core        (Expr)
import Dhall.Parser      (Src)
import Toml.Type.TOML    (TOML)


-- TODO: populate with actual errors
data CompileError
    = CompileError String

instance Show CompileError where
    show (CompileError s) = "compile error: " ++ s

instance Exception CompileError

dhallToToml :: Expr s Void -> Either CompileError TOML
dhallToToml _ = Left $ CompileError  "not implemented"

type ExprX = Expr Src Void

tomlToDhall :: ExprX -> TOML -> Either CompileError ExprX
tomlToDhall _ _ = Left $ CompileError "not implemented"

dhallToTomlMain :: IO ()
dhallToTomlMain = putStrLn "not implemented"

tomlToDhallMain :: IO ()
tomlToDhallMain = putStrLn "not implemented"

