#!/usr/bin/env bash
# CLI timings matching prelude_import.code / prelude_import.source.
# Run from this directory.
set -euo pipefail

time_dhall() {
    local file=$1
    echo "=== dhall --file ${file} ==="
    time dhall --file "${file}" > /dev/null
    echo
}

time_dhall prelude-code.dhall
time_dhall prelude-source.dhall
