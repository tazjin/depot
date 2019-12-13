{ pkgs, ... }:

pkgs.buildGo.external {
  path = "google.golang.org/genproto";
  src = builtins.fetchGit {
    url = "https://github.com/google/go-genproto";
    rev = "55e96fffbd486c27fc0d5b4468c497d0de3f2727";
  };

  deps = with pkgs.third_party; [
    gopkgs."github.com".golang.protobuf.proto.gopkg
    gopkgs."github.com".golang.protobuf.ptypes.any.gopkg
  ];
}
