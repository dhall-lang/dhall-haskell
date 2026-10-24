#!/usr/bin/env bash
# Time typecheck, default evaluation, and hash of small.dhall.
# Each command is killed after 180s. Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"
# Keep this fixture off the user's Dhall cache. The directory persists for
# the process, so a second invocation of this script is a warm run.
export XDG_CACHE_HOME="${LARGE7_CACHE:-/tmp/dhall-large7-cache}"
mkdir -p "${XDG_CACHE_HOME}"

run_limited() {
    local label=$1
    shift
    echo "=== ${label} ==="
    python3 - "$label" "$@" <<'PY' || true
import subprocess, sys, time
label = sys.argv[1]
cmd = sys.argv[2:]
start = time.perf_counter()
try:
    result = subprocess.run(cmd, timeout=180, stdout=subprocess.DEVNULL)
except subprocess.TimeoutExpired:
    elapsed = time.perf_counter() - start
    print("KILLED after %.2fs" % elapsed, file=sys.stderr)
    sys.exit(124)
elapsed = time.perf_counter() - start
print("elapsed %.2fs  exit %d" % (elapsed, result.returncode))
sys.exit(result.returncode)
PY
    echo
}

run_limited "dhall type --quiet --file small.dhall" \
    "${DHALL}" type --quiet --file small.dhall
run_limited "dhall --file small.dhall" \
    "${DHALL}" --file small.dhall
run_limited "dhall hash --file small.dhall" \
    "${DHALL}" hash --file small.dhall
