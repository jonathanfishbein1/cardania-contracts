let
  project = import ./project.nix;
  nixpkgs = import <nixpkgs> { };
  plutus-apps-shell = import /home/jonathan/Documents/plutus-apps/shell.nix;
in
project.shellFor {
  inputsFrom = [ plutus-apps-shell ];
  tools = {
    cabal = "3.6.0.0";
    hlint = "latest"; # Selects the latest version in the hackage.nix snapshot
    haskell-language-server = "latest";
  };
}
