{ pkgs, ... }:

pkgs.third_party.emacsPackagesNg.trivialBuild rec {
  pname = "dottime";
  version = "1.0";
  src = ./dottime.el;
}
