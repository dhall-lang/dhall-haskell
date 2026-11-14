#!/usr/bin/env bash
# Run every evaluation fixture that has a CLI `run.sh`, using the dhall
# executable below. Writes a tasty-bench-shaped CSV named
# results-evaluation_cli-<short-hash>.csv in the current directory.
# Pass --commit <hash> to name the file after that commit instead of HEAD.
#
# Each row is a single end-to-end CLI sample (parse + resolve + typecheck +
# normalize), so 2*Stdev is 0. These are not the Haskell phase benches.
#
# Each run.sh runs in its own subshell under `ulimit -t` (6 minutes of CPU
# time by default). That limit is per process: every dhall the script starts
# gets its own budget, and it is not a wall-clock cap. If a command is killed
# before bash `time` prints, the elapsed time is still recorded as a
# __BENCH_TIME__ sample. Later run.sh scripts run either way.
#
# This script sets a temporary XDG_CACHE_HOME and deletes it on exit. Each
# run.sh replaces that with its own temporary cache, also deleted on exit, so
# one fixture cannot warm the next and neither uses ~/.cache/dhall.
set -euo pipefail

# Path to the dhall executable. Edit this, or override with the DHALL env var.
DHALL="${DHALL:-dhall}"

# Optional: --commit <hash> names the CSV after that commit instead of HEAD.
# The hash may be short or long. The file uses the 8-character form.
COMMIT=""
while [[ $# -gt 0 ]]; do
    case "$1" in
        --commit)
            if [[ $# -lt 2 || -z "${2:-}" ]]; then
                echo "error: --commit requires a commit hash" >&2
                exit 1
            fi
            COMMIT=$2
            shift 2
            ;;
        --commit=*)
            COMMIT=${1#--commit=}
            if [[ -z "${COMMIT}" ]]; then
                echo "error: --commit requires a commit hash" >&2
                exit 1
            fi
            shift
            ;;
        *)
            echo "error: unknown argument: $1" >&2
            exit 1
            ;;
    esac
done

# CPU seconds for `ulimit -t`. Override with CPU_LIMIT_SECONDS.
CPU_LIMIT_SECONDS="${CPU_LIMIT_SECONDS:-$((6 * 60))}"

TIME_TAG='__BENCH_TIME__'
# Sentinel on the child's stdout so a killed command cannot be mistaken for it.
STATUS_MARK='__DHALL_BENCH_STATUS__'
# bash TIMEFORMAT is inherited from the environment by child scripts.
export TIMEFORMAT="${TIME_TAG} %6R"

ROOT=$(cd "$(dirname "$0")" && pwd)
if [[ -n "${COMMIT}" ]]; then
    if ! HASH=$(git -C "${ROOT}" rev-parse --short=8 --verify "${COMMIT}^{commit}" 2>/dev/null); then
        echo "error: not a commit: ${COMMIT}" >&2
        exit 1
    fi
else
    HASH=$(git -C "${ROOT}" rev-parse --short=8 HEAD)
fi
OUT="${PWD}/results-evaluation_cli-${HASH}.csv"

if [[ -f "${DHALL}" && -x "${DHALL}" ]]; then
    DHALL=$(cd "$(dirname "${DHALL}")" && pwd)/$(basename "${DHALL}")
elif command -v "${DHALL}" >/dev/null 2>&1; then
    DHALL=$(command -v "${DHALL}")
else
    echo "error: dhall executable not found: ${DHALL}" >&2
    exit 1
fi
export DHALL

SUITE_CACHE=$(mktemp -d "${TMPDIR:-/tmp}/dhall-cli-suite.XXXXXX")
export XDG_CACHE_HOME="${SUITE_CACHE}"
cleanup_suite_cache() {
    rm -rf "${SUITE_CACHE}"
}
trap cleanup_suite_cache EXIT

csv_name() {
    local group=$1
    local header=$2
    local file left
    # Prefer the run.sh label ("file3: …", "substitutions.as_code: …") so
    # fixtures that share a filename stay distinct.
    if [[ "${header}" =~ ^([^:]+): ]]; then
        left=${BASH_REMATCH[1]}
        left=${left#"${left%%[![:space:]]*}"}
        left=${left%"${left##*[![:space:]]}"}
        if [[ "${left}" == "${group}"* ]]; then
            printf '%s' "${left}"
        else
            printf '%s.%s' "${group}" "${left}"
        fi
    elif [[ "${header}" =~ --file[[:space:]]+([^[:space:]]+) ]]; then
        file=$(basename "${BASH_REMATCH[1]}")
        printf '%s.%s' "${group}" "${file}"
    else
        printf '%s.%s' "${group}" "${header}"
    fi
}

seconds_to_ps() {
    python3 -c 'import sys; print(int(round(float(sys.argv[1]) * 1e12)))' "$1"
}

mono_now() {
    python3 -c 'import time; print("%.6f" % time.monotonic())'
}

elapsed_since() {
    python3 -c 'import sys, time; print("%.6f" % (time.monotonic() - float(sys.argv[1])))' "$1"
}

record_sample() {
    local group=$1
    local header=$2
    local secs=$3
    local name ps
    if [[ -z "${header}" ]]; then
        name="${group}.unknown"
    else
        name=$(csv_name "${group}" "${header}")
    fi
    ps=$(seconds_to_ps "${secs}")
    printf '%s,%s,0\n' "${name}" "${ps}" >> "${OUT}"
}

# bash `time` prints TIME_TAG after a command dies. If the shell itself is
# killed first, close the open header with the wall time since that header.
commit_pending_sample() {
    local secs
    if [[ -z "${pending_since}" ]]; then
        return 0
    fi
    secs=$(elapsed_since "${pending_since}")
    printf '%s %s\n' "${TIME_TAG}" "${secs}"
    echo "warning: ${group}: stopped before ${TIME_TAG}; recording ${secs}s" >&2
    record_sample "${group}" "${pending_header}" "${secs}"
    pending_header=""
    pending_since=""
    failed=1
}

echo "Using dhall: ${DHALL}" >&2
echo "CPU limit: ${CPU_LIMIT_SECONDS}s per process (ulimit -t)" >&2
echo "Suite cache: ${SUITE_CACHE}" >&2
echo "Writing: ${OUT}" >&2
echo >&2

if ! ( ulimit -t "${CPU_LIMIT_SECONDS}" ); then
    echo "error: ulimit -t ${CPU_LIMIT_SECONDS} is not supported" >&2
    exit 1
fi

{
    printf 'Name,Mean (ps),2*Stdev (ps)\n'
} > "${OUT}"

failed=0
pending_header=""
pending_since=""
run_scripts=()
while IFS= read -r line; do
    run_scripts+=("${line}")
done < <(find "${ROOT}/dhall/benchmark/evaluation" -name run.sh | LC_ALL=C sort)

if [[ ${#run_scripts[@]} -eq 0 ]]; then
    echo "error: no run.sh files found under dhall/benchmark/evaluation" >&2
    exit 1
fi

for run_sh in "${run_scripts[@]}"; do
    dir=$(dirname "${run_sh}")
    group=$(basename "${dir}")
    status=""

    echo "======== ${group} ========" >&2
    while IFS= read -r line || [[ -n "${line}" ]]; do
        if [[ "${line}" == "${STATUS_MARK}"* ]]; then
            status=${line#"${STATUS_MARK}"}
            continue
        fi
        printf '%s\n' "${line}"
        if [[ "${line}" =~ ^===[[:space:]]+(.*)[[:space:]]+===[[:space:]]*$ ]]; then
            header=${BASH_REMATCH[1]}
            commit_pending_sample
            pending_header=${header}
            pending_since=$(mono_now)
        elif [[ "${line}" == "${TIME_TAG} "* ]]; then
            secs=${line#"${TIME_TAG} "}
            record_sample "${group}" "${pending_header}" "${secs}"
            pending_header=""
            pending_since=""
        fi
    done < <(
        set +e
        (
            ulimit -t "${CPU_LIMIT_SECONDS}"
            limit_status=$?
            if [[ "${limit_status}" -ne 0 ]]; then
                exit "${limit_status}"
            fi
            bash "${run_sh}"
        ) 2>&1
        printf '%s%s\n' "${STATUS_MARK}" "$?"
    )
    commit_pending_sample

    if [[ -z "${status}" ]]; then
        echo "warning: ${group}/run.sh did not report an exit status" >&2
        failed=1
    elif [[ "${status}" -ne 0 ]]; then
        echo "warning: ${group}/run.sh exited ${status}" >&2
        failed=1
    fi
    echo >&2
done

echo "Wrote ${OUT}" >&2
if [[ "${failed}" -ne 0 ]]; then
    echo "warning: one or more run.sh scripts failed; timings for commands that started are in the CSV" >&2
    exit 1
fi
