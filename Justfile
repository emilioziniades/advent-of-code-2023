alias b := build
alias t := test
alias w := watch

build:
    cabal build

test:
    cabal test

watch:
    watchexec -c -- cabal test
