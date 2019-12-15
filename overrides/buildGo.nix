{ pkgs, ... }:

(import "${builtins.fetchGit {
  url = "https://github.com/tazjin/buildGo.nix";
  rev = "53630e3b1544de38f509e48ac0f589bcfafa7728";
}}/buildGo.nix") { pkgs = pkgs.third_party; }
