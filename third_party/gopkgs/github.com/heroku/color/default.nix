{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/heroku/color";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "heroku";
    repo = "color";
    rev = "e220b10c343a04cf4de0fcf7eb58dc0648430081";
    sha256 = "05yrgmhss7iynkyj7kpdqfic5kxcnv35m6ff3c3c0s69r65h6paw";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".mattn.go-colorable
  ];
}
