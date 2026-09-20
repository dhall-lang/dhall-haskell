#!/usr/bin/env bash
# CLI timing of large2 (Haskell uses this file for normalize + CBOR encode/decode).
# Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"

echo "=== dhall --file main.dhall ==="
time "${DHALL}" --file main.dhall > /dev/null
echo
