module Main where

import qualified Criterion
import qualified Criterion.Main
import qualified Data.Text.Lazy.IO
import qualified Dhall.Parser

main :: IO ()
main = Criterion.Main.defaultMain
    [ Criterion.env (Data.Text.Lazy.IO.readFile "./tmp") $ \text ->
        Criterion.bench "tokenize" (Criterion.nf Dhall.Parser.tokenize text)
    ]
