{ pkgs, ... }:

pkgs.buildGo.external {
  path = "golang.org/x/oauth2";
  src = builtins.fetchGit {
    url = "https://go.googlesource.com/oauth2";
    rev = "0f29369cfe4552d0e4bcddc57cc75f4d7e672a33";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."golang.org".x.net.context.ctxhttp
    gopkgs."cloud.google.com".go.compute.metadata
  ];
}
