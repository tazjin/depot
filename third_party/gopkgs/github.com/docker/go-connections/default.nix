{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/docker/go-connections";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "docker";
    repo = "go-connections";
    rev = "f2a43445ca32a74e9855ddf9f84d7f9cedde55f9";
    sha256 = "10abbr8fhfd95zjbkk4k62irfswvvahihk5wgzacd9v1d9h7vv9k";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".pkg.errors
  ];
}
