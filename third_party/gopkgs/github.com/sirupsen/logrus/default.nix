{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/sirupsen/logrus";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "sirupsen";
    repo = "logrus";
    rev = "67a7fdcf741f4d5cee82cb9800994ccfd4393ad0";
    sha256 = "0wk9zgfm4m4yf3spgxkwxzsj15qxna9pjdn1hnc1n40zwvmz2204";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."golang.org".x.sys.unix
  ];
}
