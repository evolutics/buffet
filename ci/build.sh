#!/bin/bash

set -eu -o pipefail

readonly SCRIPT_FOLDER="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_FOLDER="$(dirname "${SCRIPT_FOLDER}")"

pushd "${PROJECT_FOLDER}"

docker build --tag code_checkers \
  --build-arg prettier=1.17.0 \
  .

docker run --volume "$(pwd)":/workdir code_checkers \
  prettier --check '**/*.+(json|md|yaml|yml)'

stack --system-ghc test

popd
