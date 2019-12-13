{ pkgs, ... }:

pkgs.buildGo.external {
  path = "google.golang.org/api";
  src = builtins.fetchGit {
    url = "https://code.googlesource.com/google-api-go-client";
    rev = "8b4e46d953bd748a9ff098644a42389b3d8dab41";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".googleapis.gax-go.v2
    gopkgs."golang.org".x.oauth2.google
    gopkgs."golang.org".x.oauth2
    gopkgs."google.golang.org".grpc
    gopkgs."google.golang.org".grpc.naming
    gopkgs."go.opencensus.io".plugin.ochttp
    gopkgs."go.opencensus.io".trace
    gopkgs."go.opencensus.io".trace.propagation
  ];
}
