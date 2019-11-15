# This file sets up the top-level package set by merging all local packages into
# the nixpkgs top-level.
#
# This makes packages accessible via the Nixery instance that is configured to
# use this repository as its nixpkgs source.

with builtins;

let
  # The pinned commit here is identical to the public nixery.dev
  # version, since popularity data has been generated for that.
  stableCommit = "80b42e630b23052d9525840a9742100a2ceaaa8f";
  stableSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${stableCommit}.tar.gz";
    sha256 = "0243qiivxl3z51biy4f5y5cy81x5bki5dazl9wqwgnmd373gpmxy";
  };
  readTree = import ./read-tree.nix;

  # Derivations that have `meta.enableCI` set to `true` should be
  # built by the CI system on every commit. This code implements
  # filtering of all derivations in the local sets against this
  # condition.
  filterCI = lib: pkgs: let
    inherit (lib) collect isDerivation filterAttrsRecursive;
    ciCondition = _: x: (!isDerivation x) || ((x ? meta.enableCI) && (x.meta.enableCI));
  in collect isDerivation (filterAttrsRecursive ciCondition pkgs);

  repoPkgs = self: super:
    let config = {
      pkgs = self;
      upstream = super;

      kms = {
        project = "tazjins-infrastructure";
        region = "europe-north1";
        keyring = "tazjins-keys";
        key = "kontemplate-key";
      };
    };
    in {
      services = readTree ./services config;
      tools = readTree ./tools config;
      third_party = readTree ./third_party config;
    }
    # Load overrides into the top-level:
    // (readTree ./overrides config)
    # Collect all projects that should be built by CI
    // {
      ciProjects = (filterCI super.lib self.services)
        ++ (filterCI super.lib self.tools)
        ++ (filterCI super.lib self.third_party);
    };
in { ... } @ args: import stableSrc (args // {
    overlays = [ repoPkgs ];
    config.allowUnfree = true;
    config.allowBroken = true;
})
