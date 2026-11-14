#!/usr/bin/env bash
# CLI timings matching large5.code.
# Timed on an empty cache and then again on that cache.
# Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"
# shellcheck source=../cli-cache.sh
source "${HERE}/../cli-cache.sh"
dhall_cli_cache_begin

dhall_cli_time_cold_warm pipeline-code "${DHALL}" --file pipeline-code.dhall
