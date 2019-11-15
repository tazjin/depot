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

  localPkgs = self: super:
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
    } // (readTree ./overrides config);

  #   # All projects that should be built by CI should be added here:
  #   ciProjects = [
  #     self.kontemplate
  #     self.nixery
  #     self.ormolu
  #     self.terraform-gcp
  #   ] ++ filter (d: d ? meta.broken && !d.meta.broken) (attrValues self.tazjin);
  # };

in { ... } @ args: import stableSrc (args // {
    overlays = [ localPkgs ];
    config.allowUnfree = true;
    config.allowBroken = true;
})
