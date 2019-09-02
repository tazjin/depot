# This file sets up the top-level package set by merging all local packages into
# the nixpkgs top-level.
#
# This makes packages accessible via the Nixery instance that is configured to
# use this repository as its nixpkgs source.

with builtins;

let
  # The pinned commit here is identical to the public nixery.dev
  # version, since popularity data has been generated for that.
  stableCommit = "88d9f776091896cfe57dc6fbdf246e7d27d5f105";
  stableSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${stableCommit}.tar.gz";
    sha256 = "0z8a0g69fmbbzi77jhvhwafv73dn5fg3gsr0q828lss6j5qpx995";
  };

  unstableCommit = "765a71f15025ce78024bae3dc4a92bd2be3a8fbf";
  unstableSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${unstableCommit}.tar.gz";
    sha256 = "0j1wghr9dj9njn3x9xi0wzjk1107gi2pxb0w2dk8g0djmhnlx71q";
  };
  unstable = import unstableSrc {};

  localPkgs = self: super: {
    # Local projects should be added here:
    tazjin = {
      blog = import ./services/tazblog { pkgs = self; };
      blog_cli = self.callPackage ./tools/blog_cli {};
      gemma = self.callPackage ./services/gemma {};
    };

    # Third-party projects (either vendored or modified from nixpkgs) go here:
    gitAppraise = self.callPackage ./third_party/go/git-appraise/git-appraise {};

    nixery = import ./third_party/nixery.nix { pkgs = self; };
    terraform-gcp = unstable.terraform_0_12.withPlugins(p: [ p.google p.google-beta ]);
    ormolu = import (self.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "a7076c0f83e5c06ea9067b71171859fa2ba8afd9";
      sha256 = "1p4n2ja4ciw3qfskn65ggpy37mvgf2sslxqmqn8s8jjarnqcyfny";
    }) { pkgs = self; };

    # Gemma needs an older version of Elm to be built. Updating it to
    # the newer version is a lot of effort.
    elmPackages = (import (self.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "14f9ee66e63077539252f8b4550049381a082518";
      sha256 = "1wn7nmb1cqfk2j91l3rwc6yhimfkzxprb8wknw5wi57yhq9m6lv1";
    }) {}).elmPackages;
  };

in { ... } @ args: import stableSrc (args // {
    overlays = [ localPkgs ];
    config.allowUnfree = true;
})
