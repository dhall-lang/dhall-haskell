#!/usr/bin/env bash
# CLI timing of the large4 customization tree (Haskell: large4).
# Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"

echo "=== dhall --file generate-example.dhall ==="
time "${DHALL}" --file generate-example.dhall > /dev/null
echo
