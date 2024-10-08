{
  description = "Haskell development environment for Advent of Code 2023";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/main";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            (haskellPackages.ghcWithPackages
              (pkgs:
                with pkgs; [
                  cabal-install
                  haskell-language-server
                  hlint
                  fourmolu
                ]))
            zlib
            just
            watchexec
          ];
        };
      }
    );
}
