{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/containerd/containerd";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "containerd";
    repo = "containerd";
    rev = "5473637144496250e4221c9103568d96c33ca29b";
    sha256 = "157km87ppmmz22akcdx915nq6xpacgmwr2343wqa7ph7k3m5nrjj";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".pkg.errors
    gopkgs."google.golang.org".grpc.codes
    gopkgs."google.golang.org".grpc.status
  ];
}
