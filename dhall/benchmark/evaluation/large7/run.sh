#!/usr/bin/env bash
# Time typecheck, default evaluation, and hash of big.dhall.
# Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"
# Keep this fixture off the user's Dhall cache. The directory persists for
# the process, so a second invocation of this script is a warm run.
export XDG_CACHE_HOME="${LARGE7_CACHE:-/tmp/dhall-large7-cache}"
mkdir -p "${XDG_CACHE_HOME}"

# The label before the colon is the row name. The CLI collector records
# these as large7.type, large7.default, and large7.hash.
time_dhall() {
    local label=$1
    shift
    echo "=== ${label}: ${*} ==="
    time "$@" > /dev/null
    echo
}

time_dhall type "${DHALL}" type --quiet --file big.dhall
time_dhall default "${DHALL}" --file big.dhall
time_dhall hash "${DHALL}" hash --file big.dhall

# small.dhall is Name/words.dhall, a subset of the package above. To time that
# entry instead, comment out the three big.dhall commands and uncomment these:
# time_dhall type-small "${DHALL}" type --quiet --file small.dhall
# time_dhall default-small "${DHALL}" --file small.dhall
# time_dhall hash-small "${DHALL}" hash --file small.dhall
