# Gemma needs an older version of Elm to be built. Updating it to
# the newer version is a lot of effort.
{ pkgs, ... }:

(import (pkgs.fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "14f9ee66e63077539252f8b4550049381a082518";
  sha256 = "1wn7nmb1cqfk2j91l3rwc6yhimfkzxprb8wknw5wi57yhq9m6lv1";
}) {}).elmPackages
