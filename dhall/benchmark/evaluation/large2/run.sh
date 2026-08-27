#!/usr/bin/env bash
# CLI timing of large2 (Haskell uses this file for normalize + CBOR encode/decode).
# Run from this directory.
set -euo pipefail

echo "=== dhall --file main.dhall ==="
time dhall --file main.dhall > /dev/null
echo
