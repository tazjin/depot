{ pkgs, ... }:

with pkgs.third_party.emacsPackagesNg;

melpaBuild rec {
  pname = "term-switcher";
  version = "1.0";
  src = ./term-switcher.el;
  packageRequires = [ dash ivy s vterm ];

  recipe = builtins.toFile "recipe" ''
    (term-switcher :fetcher github :repo "tazjin/depot")
  '';
}
