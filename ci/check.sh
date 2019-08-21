#!/bin/sh

set -o errexit
set -o nounset
set -o pipefail

check_with_git() {
  git diff --check HEAD^
}

check_with_gitlint() {
  gitlint --config ci/.gitlint
}

check_with_hindent() {
  git ls-files -z '*.hs' | xargs -0 hindent --sort-imports --validate
}

check_with_hlint() {
  hlint --git --hint ci/.hlint.yaml
}

check_with_hunspell() {
  git log -1 --format=%B | hunspell -l -d en_US -p ci/personal_words.dic \
    | sort | uniq | tr '\n' '\0' | xargs -0 -r -n 1 sh -c \
    'echo "Misspelling: $@"; exit 1' --
}

check_with_prettier() {
  prettier --check '**/*.+(json|md|yaml|yml)'
}

main() {
  check_with_git
  check_with_gitlint
  check_with_hindent
  check_with_hlint
  check_with_hunspell
  check_with_prettier
}

main "$@"
