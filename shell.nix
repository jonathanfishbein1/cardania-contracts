let
  project = import ./project.nix;
in
project.shellFor {
  tools = {
    cabal = "3.6.0.0";
    hlint = "latest"; # Selects the latest version in the hackage.nix snapshot
    haskell-language-server = "latest";
  };
}
