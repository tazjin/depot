{ pkgs, ... }:

pkgs.buildGo.external {
  path = "google.golang.org/genproto";
  src = builtins.fetchGit {
    url = "https://github.com/google/go-genproto";
    rev = "0243a4be9c8f1264d238fdc2895620b4d9baf9e1";
  };

  deps = with pkgs.third_party; [
    gopkgs."github.com".golang.protobuf.proto.gopkg
    gopkgs."github.com".golang.protobuf.ptypes.any.gopkg
  ];
}
