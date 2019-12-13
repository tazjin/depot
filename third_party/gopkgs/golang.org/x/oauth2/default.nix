{ pkgs, ... }:

pkgs.buildGo.external {
  path = "golang.org/x/oauth2";
  src = builtins.fetchGit {
    url = "https://go.googlesource.com/oauth2";
    rev = "858c2ad4c8b6c5d10852cb89079f6ca1c7309787";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."golang.org".x.net.context.ctxhttp
    gopkgs."cloud.google.com".go.compute.metadata
  ];
}
