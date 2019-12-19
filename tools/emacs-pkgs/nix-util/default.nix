{ pkgs, ... }:

pkgs.third_party.emacsPackagesNg.trivialBuild rec {
  pname = "nix-util";
  version = "1.0";
  src = ./nix-util.el;
}
