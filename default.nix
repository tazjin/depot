# This file sets up the top-level package set by traversing the package tree
# (see read-tree.nix for details) and constructing a matching attribute set
# tree.
#
# This makes packages accessible via the Nixery instance that is configured to
# use this repository as its nixpkgs source.

{ ... }@args:

with builtins;

let
  # This definition of fix is identical to <nixpkgs>.lib.fix, but the global
  # package set is not available here.
  fix = f: let x = f x; in x;

  # Global configuration that all packages are called with.
  config = pkgs: {
    inherit pkgs;

    kms = {
      project = "tazjins-infrastructure";
      region = "europe-north1";
      keyring = "tazjins-keys";
      key = "kontemplate-key";
    };
  };

  readTree' = import ./read-tree.nix;

  localPkgs = readTree: {
    services    = readTree ./services;
    tools       = readTree ./tools;
    third_party = readTree ./third_party;
  };
in fix(self: {
  config = config self;

  # Elevate 'lib' from nixpkgs
  lib = import (self.third_party.nixpkgsSrc + "/lib");
}

# Add local packages as structured by readTree
// (localPkgs (readTree' self.config))

# Load overrides into the top-level.
#
# This can be used to move things from third_party into the top-level, too (such
# as `lib`).
// (readTree' self.config) ./overrides
)
