#!/usr/bin/env bash
# CLI timing of the large4 customization tree (Haskell: large4).
# Run from this directory.
set -euo pipefail

echo "=== dhall --file generate-example.dhall ==="
time dhall --file generate-example.dhall > /dev/null
echo
