#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

check_code() {
  docker run --entrypoint sh --rm --volume "$(pwd)":/workdir \
    evolutics/travel-kit:0.5.0 -c \
    'git ls-files -z | xargs -0 travel-kit check --'

  local -r further_code_checkers="$(further_code_checkers_name)"
  build_further_code_checkers "${further_code_checkers}"
  docker run --rm --volume "$(pwd)":/workdir "${further_code_checkers}" \
    scripts/check_further.sh
}

further_code_checkers_name() {
  local -r hash="$(sha256sum scripts/further_code_checkers_build_arguments \
    | cut --characters -16)"
  echo "evolutics/buffet:internal-${hash}"
}

build_further_code_checkers() {
  # shellcheck disable=SC2046
  docker pull "$1" \
    || (docker build --rm=false --tag "$1" \
    $(< scripts/further_code_checkers_build_arguments) \
    && docker push "$1")
}

test_code() {
  stack --system-ghc test --ghc-options -Werror
}

main() {
  local -r script_folder="$(dirname "$(readlink --canonicalize "$0")")"
  local -r project_folder="$(dirname "${script_folder}")"

  pushd "${project_folder}"

  check_code
  test_code

  popd
}

main "$@"
