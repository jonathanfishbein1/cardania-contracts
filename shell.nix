let
  project = import ./default.nix;
in
project.shellFor {
  buildInputs = [ (import <nixpkgs> { }).git ];
  exactDeps = true;
}
