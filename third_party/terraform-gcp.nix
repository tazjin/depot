{ pkgs, ... }:

pkgs.terraform_0_12.withPlugins(p: [ p.google p.google-beta ])
