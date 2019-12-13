{ pkgs, ... }:

pkgs.buildGo.external {
  path = "google.golang.org/api";
  src = builtins.fetchGit {
    url = "https://code.googlesource.com/google-api-go-client";
    rev = "573115b31dcba90b19c25508412495d63f86e804";
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
