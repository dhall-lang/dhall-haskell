# Shared cache setup for evaluation CLI run.sh scripts.
# Source this file; do not execute it.

# Create a private cache directory and remove it when the shell exits,
# including after a failed command.
dhall_cli_cache_begin() {
    if [[ -n "${DHALL_CLI_CACHE_DIR:-}" ]]; then
        return 0
    fi
    DHALL_CLI_CACHE_DIR=$(mktemp -d "${TMPDIR:-/tmp}/dhall-cli-cache.XXXXXX")
    export XDG_CACHE_HOME="${DHALL_CLI_CACHE_DIR}"
    dhall_cli_on_exit dhall_cli_cache_cleanup
}

dhall_cli_cache_cleanup() {
    if [[ -n "${DHALL_CLI_CACHE_DIR:-}" ]]; then
        rm -rf "${DHALL_CLI_CACHE_DIR}"
    fi
}

# Register a function to run on exit. Later registrations run after earlier ones.
dhall_cli_on_exit() {
    DHALL_CLI_EXIT_HOOKS="${DHALL_CLI_EXIT_HOOKS:-} $1"
    # shellcheck disable=SC2064
    trap 'for _dhall_cli_hook in ${DHALL_CLI_EXIT_HOOKS}; do "${_dhall_cli_hook}" || true; done' EXIT
}

dhall_cli_cache_reset() {
    rm -rf "${XDG_CACHE_HOME}"
    mkdir -p "${XDG_CACHE_HOME}"
}

# Time one command. The text before the colon is the collector's row name.
dhall_cli_time() {
    local label=$1
    shift
    echo "=== ${label}: ${*} ==="
    time "$@" > /dev/null
    echo
}

# Empty the cache, time the command, then time the same command again.
dhall_cli_time_cold_warm() {
    local label=$1
    shift
    dhall_cli_cache_reset
    dhall_cli_time "${label}-cold" "$@"
    dhall_cli_time "${label}-warm" "$@"
}

# Cold then warm for a k8s-style expression passed on stdin.
dhall_cli_time_expr_cold_warm() {
    local label=$1
    local expr=$2
    dhall_cli_cache_reset
    echo "=== ${label}-cold: ${expr} ==="
    time "${DHALL}" <<< "${expr}" > /dev/null
    echo
    echo "=== ${label}-warm: ${expr} ==="
    time "${DHALL}" <<< "${expr}" > /dev/null
    echo
}
