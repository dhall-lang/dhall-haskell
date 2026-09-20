#!/usr/bin/env bash
# Cold-ish CLI timing of the large1 fixture (Haskell: large1.parse/resolve/typecheck/evaluation).
# Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"

echo "=== dhall --file main.dhall ==="
time "${DHALL}" --file main.dhall > /dev/null
echo
