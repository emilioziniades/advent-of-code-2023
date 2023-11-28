{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];

      perSystem = {
        self',
        pkgs,
        ...
      }: {
        haskellProjects.default = {
          devShell = {
            enable = true;

            mkShellArgs = {
              buildInputs = with pkgs; [
                zlib
              ];

              shellHook = ''
                cyan=$(tput setaf 6)
                reset=$(tput sgr0)
                PS1="\[$cyan\]nix\[$reset\] $PS1"
              '';
            };

            tools = hp: {
              fourmolu = hp.fourmolu;
              haskell-language-server = hp.haskell-language-server;
            };

            hlsCheck.enable = true;
          };
        };
      };
    };
}
