#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

check_code() {
  local -r code_checkers="$(code_checkers_name)"
  build_code_checkers "${code_checkers}"
  docker run --rm --volume "$(pwd)":/workdir "${code_checkers}" scripts/check.sh
}

code_checkers_name() {
  local -r hash="$(sha256sum scripts/code_checkers_build_arguments \
    | cut --characters -16)"
  echo "evolutics/buffet:internal-${hash}"
}

build_code_checkers() {
  docker pull "$1" \
    || (docker build --rm=false --tag "$1" \
    $(< scripts/code_checkers_build_arguments) \
    && docker push "$1")
}

test_code() {
  stack --system-ghc test --ghc-options -Werror
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
