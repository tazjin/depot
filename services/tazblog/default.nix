# Build configuration for the blog using plain Nix.
#
# tazblog.nix was generated using cabal2nix.

{ pkgs ? import <nixpkgs> {} }:

pkgs.haskell.packages.ghc865.callPackage ./tazblog.nix {}
