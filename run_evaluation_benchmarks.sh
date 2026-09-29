hash=$(git rev-parse --short HEAD)
stack bench dhall:bench:evaluation --ghc-options "-fproc-alignment=64" --ba "--csv results-evaluation-$hash.csv"
