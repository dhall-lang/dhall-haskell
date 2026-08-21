#!/usr/bin/env bash
# Cold-ish CLI timing of the large1 fixture (Haskell: large1.parse/resolve/typecheck/evaluation).
# Run from this directory.
set -euo pipefail

echo "=== dhall --file main.dhall ==="
time dhall --file main.dhall > /dev/null
echo
