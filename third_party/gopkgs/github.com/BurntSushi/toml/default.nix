{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/BurntSushi/toml";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "BurntSushi";
    repo = "toml";
    rev = "3012a1dbe2e4bd1391d42b32f0577cb7bbc7f005";
    sha256 = "1fjdwwfzyzllgiwydknf1pwjvy49qxfsczqx5gz3y0izs7as99j6";
  };
}
