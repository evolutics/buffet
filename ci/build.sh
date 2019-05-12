#!/bin/bash

set -eu -o pipefail

readonly SCRIPT_FOLDER="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_FOLDER="$(dirname "${SCRIPT_FOLDER}")"

pushd "${PROJECT_FOLDER}"

stack --system-ghc test

popd
