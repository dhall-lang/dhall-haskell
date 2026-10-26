#!/usr/bin/env bash
# CLI timings matching the four substitutions.* Haskell groups.
#
# Generates the many-files tree into a temp directory (same shape as
# writeManyFilesFixture in evaluation/Main.hs). Nested-let pipelines stay
# as the static files in this directory.
#
# The Haskell benches inject UserType* substitutions via InputSettings.
# The CLI cannot do that; this times dhall --file with an empty substitution
# map. Use `stack bench evaluation --ba '--pattern substitutions'` for the
# customer-shaped measurement.
#
# Each file is timed on an empty cache and then again on that cache.
#
# Runnable from any directory.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
cd "${HERE}"
DHALL="${DHALL:-dhall}"
# Keep in sync with manyFilesModuleCount in evaluation/Main.hs.
MANY_FILES_MODULE_COUNT=200
# shellcheck source=../cli-cache.sh
source "${HERE}/../cli-cache.sh"
dhall_cli_cache_begin

echo "Note: CLI has no InputSettings substitutions; this is not the Haskell substitutions.* bench."
echo

generate_many_files_tree() {
    local root=$1
    local mods="${root}/mods"
    mkdir -p "${mods}"

    local i name
    for i in $(seq 0 $((MANY_FILES_MODULE_COUNT - 1))); do
        printf -v name 'm%03d.dhall' "${i}"
        cat > "${mods}/${name}" <<EOF
let a = ${i}
let x = 1
in  a + x
EOF
    done

    {
        echo '['
        for i in $(seq 0 $((MANY_FILES_MODULE_COUNT - 1))); do
            printf -v name 'm%03d.dhall' "${i}"
            if [[ "${i}" -lt $((MANY_FILES_MODULE_COUNT - 1)) ]]; then
                echo "    ./mods/${name},"
            else
                echo "    ./mods/${name}"
            fi
        done
        echo ']'
    } > "${root}/package.dhall"

    echo './package.dhall' > "${root}/pipeline-code.dhall"
}

cleanup_many_root() {
    if [[ -n "${MANY_ROOT:-}" ]]; then
        rm -rf "${MANY_ROOT}"
    fi
}

echo "--- substitutions.as_code (nested lets) ---"
dhall_cli_time_cold_warm "substitutions.as_code" \
    "${DHALL}" --file "${HERE}/pipeline-code.dhall"

MANY_ROOT=$(mktemp -d "${TMPDIR:-/tmp}/dhall-substitutions-many-files.XXXXXX")
dhall_cli_on_exit cleanup_many_root

echo "--- substitutions.many_files.* (generating ${MANY_FILES_MODULE_COUNT} modules) ---"
generate_many_files_tree "${MANY_ROOT}"
dhall_cli_time_cold_warm "substitutions.many_files.as_code" \
    "${DHALL}" --file "${MANY_ROOT}/pipeline-code.dhall"
