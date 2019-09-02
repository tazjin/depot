# Build configuration for the blog using plain Nix.
#
# tazblog.nix was generated using cabal2nix.

{ writeShellScriptBin, haskell }:

let tazblog = haskell.packages.ghc865.callPackage ./tazblog.nix {};
in writeShellScriptBin "tazblog" ''
  export PORT=8000
  export RESOURCE_DIR=${./static}
  exec ${tazblog}/bin/tazblog
''
