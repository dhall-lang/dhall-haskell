#!/usr/bin/env bash
# CLI timings matching the k8s Haskell groups (file3 / file4 .mkPod).
# The harness evaluates an expression, not a whole file, so this uses stdin
# rather than --file. Run from this directory.
set -euo pipefail

time_expr() {
    local label=$1
    local expr=$2
    echo "=== ${label}: ${expr} ==="
    time dhall <<< "${expr}" > /dev/null
    echo
}

time_expr file3 "(./file3.dhall).mkPod"
time_expr file4 "(./file4.dhall).mkPod"
