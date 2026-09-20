#!/usr/bin/env bash
# CLI timings matching large5.code.
# Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"

time_dhall() {
    local file=$1
    echo "=== dhall --file ${file} ==="
    time "${DHALL}" --file "${file}" > /dev/null
    echo
}

time_dhall pipeline-code.dhall
