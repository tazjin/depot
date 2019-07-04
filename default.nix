# This file sets up the top-level package set by merging all local
# packages into the nixpkgs top-level.

let
  localPkgs = super: pkgs: {
    tazjin.tazblog = import ./services/tazblog { inherit pkgs; };
    tazjin.gemma = import ./services/gemma { inherit pkgs; };

    thirdParty.gitAppraise = pkgs.callPackage ./third_party/go/git-appraise/git-appraise {};
  };

  # TODO(tazjin): It might be preferable to pin a specific commit of
  # nixpkgs, but for now the assumption will be that a single release
  # channel is reasonably stable.
  nixpkgsVersion = "nixos-19.03";
  nixpkgs = "https://github.com/NixOS/nixpkgs-channels/archive/${nixpkgsVersion}.tar.gz";

in { ... } @ args: import (builtins.fetchTarball nixpkgs) (args // {
    overlays = [ localPkgs ];
})
