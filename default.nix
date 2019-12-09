# This file sets up the top-level package set by traversing the package tree
# (see read-tree.nix for details) and constructing a matching attribute set
# tree.
#
# This makes packages accessible via the Nixery instance that is configured to
# use this repository as its nixpkgs source.

with builtins;

let
  # This definition of fix is identical to <nixpkgs>.lib.fix, but the global
  # package set is not available here.
  fix = f: let x = f x; in x;

  # Derivations that have `meta.enableCI` set to `true` should be
  # built by the CI system on every commit. This code implements
  # filtering of all derivations in the local sets against this
  # condition.
  filterCI = lib: pkgs: let
    inherit (lib) collect isDerivation filterAttrsRecursive;
    ciCondition = _: x: (!isDerivation x) || ((x ? meta.enableCI) && (x.meta.enableCI));
  in collect isDerivation (filterAttrsRecursive ciCondition pkgs);

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

  # Collect all projects that should be built by CI
  # ciProjects = (filterCI self.lib self.services)
  #   ++ (filterCI super.lib self.tools)
  #   ++ (filterCI super.lib self.third_party);
  # TODO(tazjin): re-enable automatic filtering for this, requires
  # read-tree fixes
  ciProjects = with self; [
    services.tazblog
    services.nixcon-demo
    tools.kms_pass
    tools.blog_cli
  ];
}

# Add local packages as structured by readTree
// (localPkgs (readTree' self.config))

# Load overrides into the top-level.
#
# This can be used to move things from third_party into the top-level, too (such
# as `lib`).
// (readTree' self.config) ./overrides
)
