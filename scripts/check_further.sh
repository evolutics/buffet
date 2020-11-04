#!/bin/sh

set -o errexit
set -o nounset

check_with_hindent() {
  git ls-files -z '*.hs' | xargs -0 hindent --sort-imports --validate
}

check_with_hlint() {
  hlint --git --hint scripts/.hlint.yaml
}

main() {
  check_with_hindent
  check_with_hlint
}

main "$@"
