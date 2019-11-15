{ pkgs, ... }:

pkgs.third_party.naersk.buildPackage ./. {}
