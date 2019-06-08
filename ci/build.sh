#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

check_code() {
  local -r code_checkers="$(build_code_checkers | tail --lines 1)"

  check_with_git "${code_checkers}"
  check_with_gitlint "${code_checkers}"
  check_with_hindent "${code_checkers}"
  check_with_hlint "${code_checkers}"
  check_with_hunspell "${code_checkers}"
  check_with_prettier "${code_checkers}"
}

build_code_checkers() {
  local -r code_checkers_tag="$(sha256sum \
    ci/code_checkers_build_arguments Dockerfile \
    | sha256sum | cut --characters -16)"
  local -r code_checkers="evolutics/freezer_code_checkers:${code_checkers_tag}"
  docker pull "${code_checkers}" \
    || (docker build --rm=false --tag "${code_checkers}" \
    $(< ci/code_checkers_build_arguments) \
    && docker push "${code_checkers}")
  echo "${code_checkers}"
}

check_with_git() {
  docker run --volume "$(pwd)":/workdir "$1" \
    git diff --check HEAD^
}

check_with_gitlint() {
  docker run --volume "$(pwd)":/workdir "$1" \
    gitlint --config ci/.gitlint
}

check_with_hindent() {
  docker run --volume "$(pwd)":/workdir "$1" sh -c \
    "git ls-files -z '*.hs' | xargs -0 hindent --sort-imports --validate"
}

check_with_hlint() {
  docker run --volume "$(pwd)":/workdir "$1" \
    hlint --git --hint ci/.hlint.yaml .
}

check_with_hunspell() {
  docker run --volume "$(pwd)":/workdir "$1" sh -c \
    "git log -1 --format=%B | hunspell -l -d en_US -p ci/personal_words.dic \
    | sort | uniq | tr '\n' '\0' | xargs -0 -r -n 1 sh -c \
    'echo "'"Misspelling: $@"'"; exit 1' --"
}

check_with_prettier() {
  docker run --volume "$(pwd)":/workdir "$1" \
    prettier --check '**/*.+(json|md|yaml|yml)'
}

test_code() {
  stack --system-ghc test
}

main() {
  local -r script_folder="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  local -r project_folder="$(dirname "${script_folder}")"

  pushd "${project_folder}"

  check_code
  test_code

  popd
}

main "$@"
