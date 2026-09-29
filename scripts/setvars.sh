#!/usr/bin/env bash

# Set the ORION environment for the current shell.
#
# Usage:
#   source scripts/setvars.sh
#
# This script intentionally does not modify ~/.bashrc or ~/.zshrc.

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script must be sourced, not executed." >&2
    echo "Use:" >&2
    echo "  source scripts/setvars.sh" >&2
    exit 1
fi

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
export ORIONDIR="$(cd -- "${SCRIPT_DIR}/.." && pwd)"

ORION() {
    "${ORIONDIR}/bin/app/converter" "$@"
}

echo "ORION environment configured:"
echo "  ORIONDIR=${ORIONDIR}"
echo "  ORION=${ORIONDIR}/bin/app/converter"