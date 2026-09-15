#!/usr/bin/env bash
# CLI timings matching the large4 Haskell groups.
# Run from this directory.
#
# generate-example.dhall is Code-only and can OOM (~100 GB NF during resolve).
# That is an intentional failing real-world bench; do not run it here.
# generate-example-source.dhall uses `apply-all.dhall as Source` and completes.
set -euo pipefail

time_dhall() {
    local file=$1
    echo "=== dhall --file ${file} ==="
    time dhall --file "${file}" > /dev/null
    echo
}

time_dhall generate-example-source.dhall
# time_dhall generate-example.dhall  # Code; expected to OOM until a later PR
