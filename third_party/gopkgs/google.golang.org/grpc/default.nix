{ pkgs, ... }:

pkgs.buildGo.external {
  path = "google.golang.org/grpc";
  src = builtins.fetchGit {
    url = "https://github.com/grpc/grpc-go";
    rev = "085c980048876e2735d4aba8f0d5bca4d7acaaa5";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."golang.org".x.net.trace
    gopkgs."golang.org".x.net.http2
    gopkgs."golang.org".x.net.http2.hpack
    gopkgs."golang.org".x.sys.unix
    gopkgs."github.com".golang.protobuf.proto
    gopkgs."github.com".golang.protobuf.ptypes
    gopkgs."github.com".golang.protobuf.ptypes.duration
    gopkgs."github.com".golang.protobuf.ptypes.timestamp
    gopkgs."google.golang.org".genproto.googleapis.rpc.status
  ];
}
