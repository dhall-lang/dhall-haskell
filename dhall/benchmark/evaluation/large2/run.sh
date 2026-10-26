#!/usr/bin/env bash
# CLI timing of large2 (Haskell uses this file for normalize + CBOR encode/decode).
# One cold run: this file does not load an import graph.
# Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"
# shellcheck source=../cli-cache.sh
source "${HERE}/../cli-cache.sh"
dhall_cli_cache_begin

echo "=== dhall --file main.dhall ==="
time "${DHALL}" --file main.dhall > /dev/null
echo
