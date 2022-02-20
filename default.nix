let
  project = import ./project.nix;
in
project.burn.components.exes.burn
