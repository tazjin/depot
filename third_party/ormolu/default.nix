{ pkgs, ... }:

import (pkgs.third_party.fetchFromGitHub {
  owner = "tweag";
  repo = "ormolu";
  rev = "a7076c0f83e5c06ea9067b71171859fa2ba8afd9";
  sha256 = "1p4n2ja4ciw3qfskn65ggpy37mvgf2sslxqmqn8s8jjarnqcyfny";
}) { pkgs = pkgs.third_party; }
