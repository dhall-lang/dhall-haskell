#!/usr/bin/env bash
# CLI timings of the normalize/* NbE fixtures (Haskell: typecheck + evaluation).
# Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"

time_dhall() {
    local file=$1
    echo "=== dhall --file ${file} ==="
    time "${DHALL}" --file "${file}" > /dev/null
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
