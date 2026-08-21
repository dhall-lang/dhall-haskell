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
# Each timed run uses a fresh XDG_CACHE_HOME (Mode B analogue).
#
# Run from this directory, or via ./run.sh from elsewhere.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
# Keep in sync with manyFilesModuleCount in evaluation/Main.hs.
MANY_FILES_MODULE_COUNT=200

echo "Note: CLI has no InputSettings substitutions; this is not the Haskell substitutions.* bench."
echo

time_dhall() {
    local label=$1
    local file=$2
    local cache
    cache=$(mktemp -d "${TMPDIR:-/tmp}/dhall-substitutions-cache.XXXXXX")
    echo "=== ${label}: dhall --file ${file} (fresh XDG_CACHE_HOME) ==="
    (
        export XDG_CACHE_HOME="${cache}"
        time dhall --file "${file}" > /dev/null
    )
    rm -rf "${cache}"
    echo
}

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
    echo './package.dhall as Source' > "${root}/pipeline-source.dhall"
}

echo "--- substitutions.as_code / .as_source (nested lets) ---"
time_dhall "substitutions.as_code" "${HERE}/pipeline-code.dhall"
time_dhall "substitutions.as_source" "${HERE}/pipeline-source.dhall"

MANY_ROOT=$(mktemp -d "${TMPDIR:-/tmp}/dhall-substitutions-many-files.XXXXXX")
trap 'rm -rf "${MANY_ROOT}"' EXIT

echo "--- substitutions.many_files.* (generating ${MANY_FILES_MODULE_COUNT} modules) ---"
generate_many_files_tree "${MANY_ROOT}"
time_dhall "substitutions.many_files.as_code" "${MANY_ROOT}/pipeline-code.dhall"
time_dhall "substitutions.many_files.as_source" "${MANY_ROOT}/pipeline-source.dhall"
