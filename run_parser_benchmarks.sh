hash=$(git rev-parse --short HEAD)
stack bench dhall:bench:dhall-parser --ghc-options "-fproc-alignment=64" --ba "+RTS -T -RTS --csv results-parser-$hash.csv"
