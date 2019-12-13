{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/hashicorp/golang-lru";
  src = builtins.fetchGit {
    url = "https://github.com/hashicorp/golang-lru";
    rev = "7f827b33c0f158ec5dfbba01bb0b14a4541fd81d";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."golang.org".x.net.context.ctxhttp
    gopkgs."cloud.google.com".go.compute.metadata
  ];
}
