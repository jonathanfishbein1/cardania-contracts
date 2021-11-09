let
  project = import ./project.nix;
in
project.swap-on-chain.components.exes.swap-on-chain
