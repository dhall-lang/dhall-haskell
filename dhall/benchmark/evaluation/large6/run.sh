#!/usr/bin/env bash
# CLI timings for each large6 pipeline (Code and as Source, every slow_* variant).
# Run from this directory.
set -euo pipefail

if [[ ! -f slow/parse.dhall ]]; then
    echo "Generating slow/parse.dhall…"
    python3 slow/generate-parse.py
fi

time_dhall() {
    local file=$1
    echo "=== dhall --file ${file} ==="
    time dhall --file "${file}" > /dev/null
    echo
}

for variant in parse eval typecheck normalize multi walk; do
    time_dhall "pipeline-code-long-${variant}.dhall"
    time_dhall "pipeline-source-long-${variant}.dhall"
done
