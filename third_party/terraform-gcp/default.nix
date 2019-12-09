{ pkgs, ... }:

pkgs.third_party.terraform_0_12.withPlugins(p: [ p.google p.google-beta ])
