#!/usr/bin/env bash
# CLI timings matching the k8s Haskell groups (file3 / file4 .mkPod).
# Each expression is timed on an empty cache and then again on that cache.
# Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"
# shellcheck source=../cli-cache.sh
source "${HERE}/../cli-cache.sh"
dhall_cli_cache_begin

dhall_cli_time_expr_cold_warm file3 "(./file3.dhall).mkPod"
dhall_cli_time_expr_cold_warm file4 "(./file4.dhall).mkPod"
