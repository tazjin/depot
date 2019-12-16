{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/google/go-containerregistry";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "google";
    repo = "go-containerregistry";
    rev = "2f2425524973dd16acfd41b0c4ff4a9aee16b043";
    sha256 = "16ic58m8ghjsidax8x7y8llms20hm43ac3pj8afpd112m7kkvp03";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".docker.cli.cli.config
    gopkgs."github.com".docker.cli.cli.config.types
    gopkgs."golang.org".x.sync.errgroup
  ];
}
