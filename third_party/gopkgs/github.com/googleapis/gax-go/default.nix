{ pkgs, ... }:

let
  inherit (pkgs) buildGo;
  inherit (builtins) fetchGit;
in pkgs.buildGo.external {
  path = "github.com/googleapis/gax-go";
  src = fetchGit {
    url = "https://github.com/googleapis/gax-go";
    rev = "bd5b16380fd03dc758d11cef74ba2e3bc8b0e8c2";
  };

  deps = with pkgs.third_party; [
    gopkgs."golang.org".x.net.trace.gopkg
    gopkgs."google.golang.org".grpc.gopkg
    gopkgs."google.golang.org".grpc.codes.gopkg
    gopkgs."google.golang.org".grpc.status.gopkg
  ];
}
