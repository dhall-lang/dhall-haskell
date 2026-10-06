hash=$(git rev-parse --short HEAD)
stack bench dhall:bench:evaluation --ghc-options "-fproc-alignment=64" --ba "+RTS -T -RTS --csv results-evaluation-$hash.csv"
