alias b := build
alias t := test
alias w := watch
alias l := lint

default: lint build test

build opts="":
    cabal build all {{ opts }}

generate-cache-key:
    just build "--dry-run"

build-dependencies:
    just build "--only-dependencies"

test pattern="Tests":
    cabal test --test-options '-p "{{pattern}}"'

watch pattern="Tests":
    watchexec -c --shell=none -- just test '{{pattern}}'

lint:
    hlint .


