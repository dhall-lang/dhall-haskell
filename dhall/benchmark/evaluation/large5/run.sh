#!/usr/bin/env bash
# CLI timings matching large5.code.
# Run from this directory.
set -euo pipefail

time_dhall() {
    local file=$1
    echo "=== dhall --file ${file} ==="
    time dhall --file "${file}" > /dev/null
    echo
}

time_dhall pipeline-code.dhall
