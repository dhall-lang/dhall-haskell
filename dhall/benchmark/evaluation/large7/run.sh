#!/usr/bin/env bash
# Time typecheck, default evaluation, and hash of big.dhall.
# Each command is timed on an empty cache and then again on that cache.
# Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"
# shellcheck source=../cli-cache.sh
source "${HERE}/../cli-cache.sh"
dhall_cli_cache_begin

# The label before the colon is the row name. The CLI collector records
# these as large7.type-cold, large7.type-warm, and likewise for default and hash.
dhall_cli_time_cold_warm type "${DHALL}" type --quiet --file big.dhall
dhall_cli_time_cold_warm default "${DHALL}" --file big.dhall
dhall_cli_time_cold_warm hash "${DHALL}" hash --file big.dhall

# small.dhall is Name/words.dhall, a subset of the package above. To time that
# entry instead, comment out the three big.dhall commands and uncomment these:
# dhall_cli_time_cold_warm type-small "${DHALL}" type --quiet --file small.dhall
# dhall_cli_time_cold_warm default-small "${DHALL}" --file small.dhall
# dhall_cli_time_cold_warm hash-small "${DHALL}" hash --file small.dhall
