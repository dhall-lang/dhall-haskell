#!/usr/bin/env bash
# CLI timings of the normalize/* NbE fixtures (Haskell: typecheck + evaluation).
# Run from this directory.
set -euo pipefail

time_dhall() {
    local file=$1
    echo "=== dhall --file ${file} ==="
    time dhall --file "${file}" > /dev/null
    echo
}

time_dhall ChurchEval.dhall
time_dhall FunCompose.dhall
time_dhall Iterate.dhall
time_dhall IterateAlt.dhall
time_dhall IterateAlt2.dhall
time_dhall ListBench.dhall
time_dhall ListBenchAlt.dhall
time_dhall NaturalFoldShortcut.dhall
