#!/usr/bin/env bash
# CLI timings matching the large3 Haskell groups.
# Each file is timed on an empty cache and then again on that cache.
# Runnable from any directory. These can take tens of seconds.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"
# shellcheck source=../cli-cache.sh
source "${HERE}/../cli-cache.sh"
dhall_cli_cache_begin

dhall_cli_time_cold_warm pipeline "${DHALL}" --file pipeline.dhall
dhall_cli_time_cold_warm get_config "${DHALL}" --file get_config.dhall
