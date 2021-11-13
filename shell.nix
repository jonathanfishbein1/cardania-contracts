let
  project = import ./project.nix;
  nixpkgs = import <nixpkgs> { };
in
project.shellFor {
  buildInputs = [
    nixpkgs.git
  ];
  tools = {
    cabal = "3.6.0.0";
    hlint = "latest"; # Selects the latest version in the hackage.nix snapshot
    haskell-language-server = "latest";
  };
}
