#!/usr/bin/env bash
# CLI timings matching the large3 Haskell groups.
# Runnable from any directory. These can take tens of seconds.
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

time_dhall pipeline.dhall
time_dhall get_config.dhall
