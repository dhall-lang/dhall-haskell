hash=$(git rev-parse --short HEAD)
stack bench dhall:bench:evaluation --ba "--csv results-evaluation-$hash.csv"
