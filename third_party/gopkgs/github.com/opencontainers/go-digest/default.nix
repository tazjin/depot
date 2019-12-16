{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/opencontainers/go-digest";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "opencontainers";
    repo = "go-digest";
    rev = "e9a29da4f419356054dd9f43b5a1b2f403cefcbc";
    sha256 = "1cdh2jvsn7y95a7i9j0m5rk1prhkzsddir8wr5g8v2cv87sl0gcj";
  };
}
