let
  project = import ./project.nix;
  nixpkgs = import <nixpkgs> { };
in
project.shellFor {
  buildInputs = [
    nixpkgs.haskellPackages.filemanip
  ];
  exactDeps = true;
}
