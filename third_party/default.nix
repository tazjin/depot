# This file controls the import of external dependencies (i.e.
# third-party code) into my package tree.
#
# This includes *all packages needed from nixpkgs*.

{ pkgs, ... }:
let
  # The pinned commit here is identical to the public nixery.dev
  # version, since popularity data has been generated for that.
  stableCommit = "3140fa89c51233397f496f49014f6b23216667c2";
  stableSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${stableCommit}.tar.gz";
    sha256 = "18p0d5lnfvzsyfah02mf6bi249990pfwnylwhqdh8qi70ncrk3f8";
  };
  nixpkgs = import stableSrc {
    config.allowUnfree = true;
    config.allowBroken = true;
  };

  exposed = {
    # Inherit the packages from nixpkgs that should be available inside
    # of the repo. They become available under `pkgs.third_party.<name>`
    inherit (nixpkgs)
      bashInteractive
      buildGoPackage
      cacert
      cachix
      cargo
      cgit
      coreutils
      darwin
      dockerTools
      emacs26
      emacs26-nox
      emacsPackagesNg
      emacsPackagesNgGen
      fetchFromGitHub
      fetchurl
      git
      glibc
      gnutar
      go
      google-cloud-sdk
      gzip
      haskell
      iana-etc
      jq
      kontemplate
      lib
      lispPackages
      llvmPackages
      makeWrapper
      mdbook
      moreutils
      nano
      nix
      notmuch
      openssh
      openssl
      protobuf
      remarshal
      rink
      ripgrep
      rsync
      runCommand
      rustPlatform
      rustc
      sbcl
      stdenv
      stern
      symlinkJoin
      terraform_0_12
      thttpd
      tree
      writeShellScriptBin
      writeText
      writeTextFile
      zlib
      zstd;
  };

in exposed // {
  callPackage = nixpkgs.lib.callPackageWith exposed;
  # Provide the source code of nixpkgs, but do not provide an imported
  # version of it.
  nixpkgsSrc = stableSrc;
}
