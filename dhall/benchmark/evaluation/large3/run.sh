#!/usr/bin/env bash
# CLI timings matching the large3 Haskell groups.
# Run from this directory. These can take tens of seconds.
set -euo pipefail

time_dhall() {
    local file=$1
    echo "=== dhall --file ${file} ==="
    time dhall --file "${file}" > /dev/null
    echo
}

time_dhall pipeline.dhall
time_dhall get_config.dhall
