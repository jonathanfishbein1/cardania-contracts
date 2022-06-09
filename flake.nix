{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, flake-compat }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            cardaniaContractsProject =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8104";
                shell.tools = {
                  cabal = { };
                  hlint = { };
                  haskell-language-server = { };
                };
                shell.buildInputs = with pkgs; [
                  nixpkgs-fmt
                ];
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.cardaniaContractsProject.flake { };
      in
      flake // {
        defaultPackage = flake.packages."rad-sale-on-chain:exe:rad-sale-on-chain";
      });
}
