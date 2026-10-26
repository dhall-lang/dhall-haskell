#!/usr/bin/env bash
# CLI timings for each large6 Code pipeline (every slow_* variant).
# Each file is timed on an empty cache and then again on that cache.
# Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"
# shellcheck source=../cli-cache.sh
source "${HERE}/../cli-cache.sh"
dhall_cli_cache_begin

echo "Generating slow/parse.dhall…"
python3 slow/generate-parse.py

for variant in parse eval typecheck normalize multi walk; do
    dhall_cli_time_cold_warm "pipeline-code-long-${variant}" \
        "${DHALL}" --file "pipeline-code-long-${variant}.dhall"
done
