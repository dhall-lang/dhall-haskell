let Gen = https://github.com/pgenie-io/haskell.gen/releases/download/v0.2.1/resolved.dhall

let input = ./input.dhall

in Gen.compile (None Gen.Config) input
