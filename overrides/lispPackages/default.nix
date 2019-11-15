# One of Gemma's dependencies is missing in nixpkgs' Quicklisp
# package set, it is overlaid locally here.
{ pkgs, upstream, ... }:

import ./quicklisp.nix {
  inherit (pkgs) lib;
  inherit (upstream) lispPackages;
}
