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
      gemma = pkgs.callPackage ./services/gemma {};
    };

    # Third-party projects (either vendored or modified from nixpkgs) go here:
    gitAppraise = pkgs.callPackage ./third_party/go/git-appraise/git-appraise {};
    nixery = import ./third_party/nixery.nix { pkgs = super; };
    terraform-gcp = pkgs.terraform_0_12.withPlugins(p: [ p.google ]);
    ormolu = import (super.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "a7076c0f83e5c06ea9067b71171859fa2ba8afd9";
      sha256 = "1p4n2ja4ciw3qfskn65ggpy37mvgf2sslxqmqn8s8jjarnqcyfny";
    }) { pkgs = super; };

    # Gemma needs an older version of Elm to be built. Updating it to
    # the newer version is a lot of effort.
    elmPackages = (import (super.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "14f9ee66e63077539252f8b4550049381a082518";
      sha256 = "1wn7nmb1cqfk2j91l3rwc6yhimfkzxprb8wknw5wi57yhq9m6lv1";
    }) {}).elmPackages;
  };

  # The pinned commit here is identical to the public nixery.dev
  # version, since popularity data has been generated for that.
  nixpkgsVersion = "88d9f776091896cfe57dc6fbdf246e7d27d5f105";
  nixpkgs = "https://github.com/NixOS/nixpkgs-channels/archive/${nixpkgsVersion}.tar.gz";

in { ... } @ args: import (fetchTarball nixpkgs) (args // {
    overlays = [ localPkgs ];
    config.allowUnfree = true;
})
