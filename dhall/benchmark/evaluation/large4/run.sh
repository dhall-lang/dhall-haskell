#!/usr/bin/env bash
# CLI timing of the large4 customization tree.
# Timed on an empty cache and then again on that cache.
# Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"
# shellcheck source=../cli-cache.sh
source "${HERE}/../cli-cache.sh"
dhall_cli_cache_begin

dhall_cli_time_cold_warm generate-example "${DHALL}" --file generate-example.dhall
