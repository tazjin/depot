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

  unstableCommit = "765a71f15025ce78024bae3dc4a92bd2be3a8fbf";
  unstableSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${unstableCommit}.tar.gz";
    sha256 = "0j1wghr9dj9njn3x9xi0wzjk1107gi2pxb0w2dk8g0djmhnlx71q";
  };
  unstable = import unstableSrc {};

  localPkgs = self: super: {
    # Local projects should be added here:
    tazjin = {
      blog = self.callPackage ./services/tazblog {};
      blog_cli = self.callPackage ./tools/blog_cli {};
      gemma = self.callPackage ./services/gemma {};
      nixcon = self.naersk.buildPackage ./services/nixcon-demo {};

      kms_pass = self.callPackage ./tools/kms_pass {
        project = "tazjins-infrastructure";
        region = "europe-north1";
        keyring = "tazjins-keys";
        key = "kontemplate-key";
      };
    };

    # Third-party projects (either vendored or modified from nixpkgs) go here:
    nixery = import ./third_party/nixery.nix { pkgs = self; };
    terraform-gcp = self.terraform_0_12.withPlugins(p: [ p.google p.google-beta ]);
    ormolu = import (self.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "a7076c0f83e5c06ea9067b71171859fa2ba8afd9";
      sha256 = "1p4n2ja4ciw3qfskn65ggpy37mvgf2sslxqmqn8s8jjarnqcyfny";
    }) { pkgs = self; };
    naersk = self.callPackage (self.fetchFromGitHub {
      owner = "nmattia";
      repo = "naersk";
      rev = "68c1c2b2b661913cdc5ecabea518dfdc4f449027";
      sha256 = "1ll310pl44kdbwfslzwvg2v7khf1y0xkg2j5wcfia4k7sj6bcl28";
    }) {};

    # Gemma needs an older version of Elm to be built. Updating it to
    # the newer version is a lot of effort.
    elmPackages = (import (self.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "14f9ee66e63077539252f8b4550049381a082518";
      sha256 = "1wn7nmb1cqfk2j91l3rwc6yhimfkzxprb8wknw5wi57yhq9m6lv1";
    }) {}).elmPackages;

    # Wrap kontemplate to inject the Cloud KMS version of 'pass'
    kontemplate =
      let master = super.kontemplate.overrideAttrs(_: {
        src = self.fetchFromGitHub {
          owner = "tazjin";
          repo = "kontemplate";
          rev = "v1.8.0";
          sha256 = "123mjmmm4hynraq1fpn3j5i0a1i87l265kkjraxxxbl0zacv74i1";
        };
      });
      in self.writeShellScriptBin "kontemplate" ''
        export PATH="${self.tazjin.kms_pass}/bin:$PATH"
        exec ${master}/bin/kontemplate $@
      '';

    # One of Gemma's dependencies is missing in nixpkgs' Quicklisp
    # package set, it is overlaid locally here.
    lispPackages = import ./third_party/common_lisp/quicklisp.nix {
      inherit (self) lib;
      inherit (super) lispPackages;
    };

    # All projects that should be built by CI should be added here:
    ciProjects = [
      self.kontemplate
      self.nixery
      self.ormolu
      self.terraform-gcp
    ] ++ filter (d: d ? meta.broken && !d.meta.broken) (attrValues self.tazjin);
  };

in { ... } @ args: import stableSrc (args // {
    overlays = [ localPkgs ];
    config.allowUnfree = true;
    config.allowBroken = true;
})
