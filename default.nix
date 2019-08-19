# This file sets up the top-level package set by merging all local packages into
# the nixpkgs top-level.
#
# This makes packages accessible via the Nixery instance that is configured to
# use this repository as its nixpkgs source.

with builtins;

let
  localPkgs = super: pkgs: {
    # Local projects should be added here:
    tazjin = {
      blog = import ./services/tazblog { inherit pkgs; };
      blog_cli = pkgs.callPackage ./tools/blog_cli {};
      gemma = import ./services/gemma { inherit pkgs; };
    };

    # Third-party projects (either vendored or modified from nixpkgs) go here:
    gitAppraise = pkgs.callPackage ./third_party/go/git-appraise/git-appraise {};
    nixery = import ./third_party/nixery.nix { pkgs = super; };
    terraform-gcp = pkgs.terraform_0_12.withPlugins(p: [ p.google ]);
  };

  # The pinned commit here is identical to the public nixery.dev
  # version, since popularity data has been generated for that.
  nixpkgsVersion = "88d9f776091896cfe57dc6fbdf246e7d27d5f105";
  nixpkgs = "https://github.com/NixOS/nixpkgs-channels/archive/${nixpkgsVersion}.tar.gz";

in { ... } @ args: import (fetchTarball nixpkgs) (args // {
    overlays = [ localPkgs ];
    config.allowUnfree = true;
})
