{ pkgs, ... }:

let inherit (pkgs) callPackage fetchFromGitHub;
in callPackage (fetchFromGitHub {
  owner = "nmattia";
  repo = "naersk";
  rev = "68c1c2b2b661913cdc5ecabea518dfdc4f449027";
  sha256 = "1ll310pl44kdbwfslzwvg2v7khf1y0xkg2j5wcfia4k7sj6bcl28";
}) {}
