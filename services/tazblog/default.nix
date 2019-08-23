# Build configuration for the blog using plain Nix.
#
# tazblog.nix was generated using cabal2nix.

{ pkgs ? import <nixpkgs> {} }:

let tazblog = pkgs.haskell.packages.ghc865.callPackage ./tazblog.nix {};
in pkgs.writeShellScriptBin "tazblog" ''
  export PORT=8000
  export RESOURCE_DIR=${./static}
  exec ${tazblog}/bin/tazblog
''
