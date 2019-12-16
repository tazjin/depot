{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/spf13/cobra";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "spf13";
    repo = "cobra";
    rev = "b04b5bfc50cbb6c036d2115ed55ea1bccdaf82a9";
    sha256 = "0kndfiyb9y1vczlwi8q5c2mxsh2jliggy2cb6bb2dqvv8x8s2crk";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".spf13.pflag
  ];
}
