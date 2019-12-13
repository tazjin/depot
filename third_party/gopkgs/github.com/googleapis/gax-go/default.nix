{ pkgs, ... }:

let
  inherit (pkgs) buildGo;
  inherit (builtins) fetchGit;
in pkgs.buildGo.external {
  path = "github.com/googleapis/gax-go";
  src = fetchGit {
    url = "https://github.com/googleapis/gax-go";
    rev = "b443e5a67ec8eeac76f5f384004931878cab24b3";
  };

  deps = with pkgs.third_party; [
    gopkgs."golang.org".x.net.trace.gopkg
    gopkgs."google.golang.org".grpc.gopkg
    gopkgs."google.golang.org".grpc.codes.gopkg
    gopkgs."google.golang.org".grpc.status.gopkg
  ];
}
