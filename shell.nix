let
  project = import ./default.nix;
  nixpkgs = import <nixpkgs> { };
in
project.shellFor {
  buildInputs = [
    nixpkgs.git
    nixpkgs.haskellPackages.happy
    nixpkgs.haskellPackages.microlens-th
  ];
  exactDeps = true;
  tools = {
    cabal = "3.6.0.0";
    hlint = "latest";
    haskell-language-server = "latest";
  };
  packages = ps: with ps; [
    swap-on-chain
  ];
}
