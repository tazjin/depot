#!/usr/bin/env bash
# This script dispatches invocations transparently to programs instantiated from
# Nix.
#
# To add a new tool, in
set -ueo pipefail

readonly REPO_ROOT=$(git rev-parse --show-toplevel)
readonly ARGS="$@"
readonly TARGET_TOOL=$(basename $0)

function nix_dispatch() {
  local attr="${1}"
  local result=$(nix-build --no-out-link --attr "${attr}" "${REPO_ROOT}")

  PATH="${result}/bin:$PATH"
  if [ -z "${ARGS}" ]; then
    exec "${TARGET_TOOL}"
  else
    exec "${TARGET_TOOL}" "${ARGS}"
  fi
}

case "${TARGET_TOOL}" in
  git-appraise)
    nix_dispatch "thirdParty.gitAppraise"
    ;;
  *)
    echo "The tool '${TARGET_TOOL}' is currently not installed in this repository."
    ;;
esac
