{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/mattn/go-colorable";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "mattn";
    repo = "go-colorable";
    rev = "98ec13f34aabf44cc914c65a1cfb7b9bc815aef1";
    sha256 = "1yxcz08kminqr1221zxpibnbzfcgs3fafin0z9zqb3gqvf74jywz";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".mattn.go-isatty
  ];
}
