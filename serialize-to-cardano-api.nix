let
  project = import ./project.nix;
in
project.serialize-to-cardano-api.components.exes.token-name
