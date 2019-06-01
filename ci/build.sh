#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

readonly SCRIPT_FOLDER="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_FOLDER="$(dirname "${SCRIPT_FOLDER}")"

pushd "${PROJECT_FOLDER}"

readonly CODE_CHECKERS_TAG="$(sha256sum \
  ci/code_checkers_build_arguments Dockerfile \
  | sha256sum | cut --characters -16)"
readonly CODE_CHECKERS="evolutics/freezer_code_checkers:${CODE_CHECKERS_TAG}"
docker pull "${CODE_CHECKERS}" || true
docker build --cache-from "${CODE_CHECKERS}" \
  --tag "${CODE_CHECKERS}" $(< ci/code_checkers_build_arguments)
docker push "${CODE_CHECKERS}" || true

docker run --volume "$(pwd)":/workdir "${CODE_CHECKERS}" \
  git diff --check HEAD^

docker run --volume "$(pwd)":/workdir "${CODE_CHECKERS}" \
  gitlint --config ci/.gitlint

docker run --volume "$(pwd)":/workdir "${CODE_CHECKERS}" sh -c \
  "git ls-files -z '*.hs' | xargs -0 hindent --sort-imports --validate"

docker run --volume "$(pwd)":/workdir "${CODE_CHECKERS}" \
  hlint --git --hint ci/.hlint.yaml .

docker run --volume "$(pwd)":/workdir "${CODE_CHECKERS}" sh -c \
  "git log -1 --format=%B | hunspell -l -d en_US -p ci/personal_words.dic \
  | sort | uniq | tr '\n' '\0' | xargs -0 -r -n 1 sh -c \
  'echo "'"Misspelling: $@"'"; exit 1' --"

docker run --volume "$(pwd)":/workdir "${CODE_CHECKERS}" \
  prettier --check '**/*.+(json|md|yaml|yml)'

stack --system-ghc test

popd
