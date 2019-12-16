{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/docker/docker";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "docker";
    repo = "docker";
    rev = "3452f136aa680530ca40c03b8f0f9bd19fce056c";
    sha256 = "1sbf2sj8n2217rk4822wndi0k4qaqmywqjcv62ryljdcfz3w39zg";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".containerd.containerd.errdefs
    gopkgs."github.com".docker.distribution.reference
    gopkgs."github.com".docker.distribution.registry.api.errcode
    gopkgs."github.com".docker.go-connections.nat
    gopkgs."github.com".docker.go-connections.sockets
    gopkgs."github.com".docker.go-connections.tlsconfig
    gopkgs."github.com".docker.go-units
    gopkgs."github.com".gogo.protobuf.proto
    gopkgs."github.com".morikuni.aec
    gopkgs."github.com".opencontainers.go-digest
    gopkgs."github.com".opencontainers.image-spec.specs-go.v1
    gopkgs."github.com".pkg.errors
    gopkgs."github.com".sirupsen.logrus
    gopkgs."golang.org".x.sys.unix
    gopkgs."google.golang.org".grpc.codes
    gopkgs."google.golang.org".grpc.status
  ];
}
