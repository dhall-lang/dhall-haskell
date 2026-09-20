#!/usr/bin/env bash
# Run every evaluation fixture that has a CLI `run.sh`, using the dhall
# executable below. Writes a tasty-bench-shaped CSV named
# evaluation-results-<short-hash>.csv in the current directory.
#
# Each row is a single end-to-end CLI sample (parse + resolve + typecheck +
# normalize), so 2*Stdev is 0. These are not the Haskell phase benches.
set -euo pipefail

# Path to the dhall executable. Edit this, or override with the DHALL env var.
DHALL="${DHALL:-dhall}"

TIME_TAG='__BENCH_TIME__'
# bash TIMEFORMAT is inherited from the environment by child scripts.
export TIMEFORMAT="${TIME_TAG} %6R"

ROOT=$(cd "$(dirname "$0")" && pwd)
HASH=$(git -C "${ROOT}" rev-parse --short=8 HEAD)
OUT="${PWD}/evaluation-results-${HASH}.csv"

if [[ -f "${DHALL}" && -x "${DHALL}" ]]; then
    DHALL=$(cd "$(dirname "${DHALL}")" && pwd)/$(basename "${DHALL}")
elif command -v "${DHALL}" >/dev/null 2>&1; then
    DHALL=$(command -v "${DHALL}")
else
    echo "error: dhall executable not found: ${DHALL}" >&2
    exit 1
fi
export DHALL

WRAP=$(mktemp -d "${TMPDIR:-/tmp}/dhall-cli-bench-wrap.XXXXXX")
trap 'rm -rf "${WRAP}"' EXIT

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

echo "Using dhall: ${DHALL}" >&2
echo "Writing: ${OUT}" >&2
echo >&2

{
    printf 'Name,Mean (ps),2*Stdev (ps)\n'
} > "${OUT}"

failed=0
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
    log=$(mktemp "${WRAP}/log.XXXXXX")

    echo "======== ${group} ========" >&2
    set +e
    bash "${run_sh}" 2>&1 | tee "${log}"
    status=${PIPESTATUS[0]}
    set -e

    if [[ "${status}" -ne 0 ]]; then
        echo "warning: ${group}/run.sh exited ${status}" >&2
        failed=1
    fi

    header=""
    while IFS= read -r line || [[ -n "${line}" ]]; do
        if [[ "${line}" =~ ^===[[:space:]]+(.*)[[:space:]]+===[[:space:]]*$ ]]; then
            header=${BASH_REMATCH[1]}
        elif [[ "${line}" == "${TIME_TAG} "* ]]; then
            secs=${line#"${TIME_TAG} "}
            if [[ -z "${header}" ]]; then
                name="${group}.unknown"
            else
                name=$(csv_name "${group}" "${header}")
            fi
            ps=$(seconds_to_ps "${secs}")
            printf '%s,%s,0\n' "${name}" "${ps}" >> "${OUT}"
            header=""
        fi
    done < "${log}"
    rm -f "${log}"
    echo >&2
done

echo "Wrote ${OUT}" >&2
if [[ "${failed}" -ne 0 ]]; then
    echo "warning: one or more run.sh scripts failed; CSV may be incomplete" >&2
    exit 1
fi
