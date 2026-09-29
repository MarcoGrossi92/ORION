#!/usr/bin/env bash

# Bump the ORION version in scripts/version.txt.
#
# Usage:
#   ./scripts/version_bump.sh --major
#   ./scripts/version_bump.sh --minor
#   ./scripts/version_bump.sh --patch
#
# This script intentionally does NOT:
#   - create commits
#   - create/delete tags
#   - push to GitHub
#
# Those operations should remain explicit Git operations.

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
VERSION_FILE="${REPO_ROOT}/scripts/version.txt"

increment_version() {
    local version="$1"
    local part="$2"

    IFS='.' read -r major minor patch <<< "${version}"

    case "${part}" in
        major)
            printf '%d.0.0\n' "$((major + 1))"
            ;;
        minor)
            printf '%d.%d.0\n' "${major}" "$((minor + 1))"
            ;;
        patch)
            printf '%d.%d.%d\n' "${major}" "${minor}" "$((patch + 1))"
            ;;
        *)
            echo "Error: invalid version component '${part}'." >&2
            echo "Usage: $0 --major|--minor|--patch" >&2
            exit 1
            ;;
    esac
}

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 --major|--minor|--patch" >&2
    exit 1
fi

if [[ ! -f "${VERSION_FILE}" ]]; then
    echo "Error: version file not found: ${VERSION_FILE}" >&2
    exit 1
fi

current_version="$(tr -d '[:space:]' < "${VERSION_FILE}")"

if [[ ! "${current_version}" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    echo "Error: invalid current version '${current_version}'." >&2
    echo "Expected MAJOR.MINOR.PATCH." >&2
    exit 1
fi

case "$1" in
    --major)
        new_version="$(increment_version "${current_version}" major)"
        ;;
    --minor)
        new_version="$(increment_version "${current_version}" minor)"
        ;;
    --patch)
        new_version="$(increment_version "${current_version}" patch)"
        ;;
    *)
        echo "Error: unknown option '$1'." >&2
        echo "Usage: $0 --major|--minor|--patch" >&2
        exit 1
        ;;
esac

if git rev-parse --verify --quiet "refs/tags/v${new_version}" >/dev/null; then
    echo "Error: tag v${new_version} already exists." >&2
    exit 1
fi

printf '%s\n' "${new_version}" > "${VERSION_FILE}"

echo "Version bumped: ${current_version} -> ${new_version}"
echo
echo "Next steps:"
echo "  git diff"
echo "  git add ${VERSION_FILE}"
echo "  git commit -m \"Bump version to ${new_version}\""
echo "  git tag -a v${new_version} -m \"ORION v${new_version}\""
echo "  git push origin main"
echo "  git push origin v${new_version}"
