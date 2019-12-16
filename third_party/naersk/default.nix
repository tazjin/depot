{ pkgs, ... }:

let inherit (pkgs.third_party) callPackage fetchFromGitHub;
in callPackage (fetchFromGitHub {
  owner = "nmattia";
  repo = "naersk";
  rev = "551a2a63399589f97f503ddd8919f27bb2406354";
  sha256 = "1jrrj4qjwgqa3yjyr0apsz8hlq28rv77ll2w4xmjg2wf4z2fgj0h";
}) {}
