{ pkgs, ... }:

(import "${builtins.fetchGit {
  url = "https://github.com/tazjin/buildGo.nix";
  rev = "7f74980457df843aea542510a406f34366e8b868";
}}/buildGo.nix") { pkgs = pkgs.third_party; }
