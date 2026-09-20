#!/usr/bin/env bash
# CLI timings for each large6 Code pipeline (every slow_* variant).
# Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"

echo "Generating slow/parse.dhall…"
python3 slow/generate-parse.py

time_dhall() {
    local file=$1
    echo "=== dhall --file ${file} ==="
    time "${DHALL}" --file "${file}" > /dev/null
    echo
}

for variant in parse eval typecheck normalize multi walk; do
    time_dhall "pipeline-code-long-${variant}.dhall"
done
