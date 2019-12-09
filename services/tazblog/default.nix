# Build configuration for the blog using plain Nix.
#
# tazblog.nix was generated using cabal2nix.

{ pkgs, ... }:

let
  inherit (pkgs.third_party) writeShellScriptBin haskell;
  tazblog = haskell.packages.ghc865.callPackage ./tazblog.nix {};
  wrapper =  writeShellScriptBin "tazblog" ''
    export PORT=8000
    export RESOURCE_DIR=${./static}
    exec ${tazblog}/bin/tazblog
  '';
in wrapper.overrideAttrs(_: {
  allowSubstitutes = true;
  meta.enableCI = true;
})
