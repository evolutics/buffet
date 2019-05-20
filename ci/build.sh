#!/bin/bash

set -eu -o pipefail

readonly SCRIPT_FOLDER="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_FOLDER="$(dirname "${SCRIPT_FOLDER}")"

pushd "${PROJECT_FOLDER}"

docker build --tag code_checkers \
  --build-arg git=2.20.1-r0 \
  --build-arg gitlint=0.11.0 \
  --build-arg hlint=2.1.21 \
  --build-arg hunspell=1.6.2-r1 \
  --build-arg prettier=1.17.0 \
  .

readonly COMMITS_TO_CHECK=origin/master..HEAD

docker run --volume "$(pwd)":/workdir code_checkers sh -c \
  "git rev-list --reverse ${COMMITS_TO_CHECK} \
  | xargs -n 1 -I {} git diff --check {}^ {}"

docker run --volume "$(pwd)":/workdir code_checkers \
  gitlint --config ci/.gitlint --commits "${COMMITS_TO_CHECK}"

docker run --volume "$(pwd)":/workdir code_checkers \
  hlint --git .

docker run --volume "$(pwd)":/workdir code_checkers sh -c \
  "git log --format=%B ${COMMITS_TO_CHECK} \
  | hunspell -l -d en_US -p ci/personal_words.dic | sort | uniq \
  | tr '\n' '\0' | xargs -0 -r -n 1 sh -c \
  'echo "'"Misspelling: $@"'"; exit 1' --"

docker run --volume "$(pwd)":/workdir code_checkers \
  prettier --check '**/*.+(json|md|yaml|yml)'

stack --system-ghc test

popd
