#!/bin/bash

set -eu -o pipefail

readonly SCRIPT_FOLDER="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_FOLDER="$(dirname "${SCRIPT_FOLDER}")"

pushd "${PROJECT_FOLDER}"

docker build --tag code_checkers \
  --build-arg git=2.20.1-r0 \
  --build-arg gitlint=0.11.0 \
  --build-arg prettier=1.17.0 \
  .

readonly COMMITS_TO_CHECK=origin/master..HEAD

docker run --volume "$(pwd)":/workdir code_checkers sh -c \
  "git rev-list --reverse ${COMMITS_TO_CHECK} \
  | xargs -n 1 -I {} git diff --check {}^ {}"

docker run --volume "$(pwd)":/workdir code_checkers \
  gitlint --config ci/.gitlint --commits "${COMMITS_TO_CHECK}"

docker run --volume "$(pwd)":/workdir code_checkers \
  prettier --check '**/*.+(json|md|yaml|yml)'

stack --system-ghc test

popd
