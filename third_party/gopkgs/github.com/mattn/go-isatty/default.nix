{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/mattn/go-isatty";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "mattn";
    repo = "go-isatty";
    rev = "31745d66dd679ac0ac4f8d3ecff168fce6170c6a";
    sha256 = "0h671sv7hfprja495kavazkalkx7xzaqksjh13brcnwq67ijrali";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."golang.org".x.sys.unix
  ];
}
