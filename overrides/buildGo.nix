{ pkgs, ... }:

(import "${builtins.fetchGit {
  url = "https://github.com/tazjin/buildGo.nix";
  rev = "5649c75d01dc9058d9a6092ad2a504669cc780a9";
}}/buildGo.nix") { pkgs = pkgs.third_party; }
