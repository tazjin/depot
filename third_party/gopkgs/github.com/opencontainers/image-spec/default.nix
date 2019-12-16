{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/opencontainers/image-spec";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "opencontainers";
    repo = "image-spec";
    rev = "775207bd45b6cb8153ce218cc59351799217451f";
    sha256 = "02nd0bmkcr9mcnw7dii5s4k5n7labgwqgrr542wp1m4k9i5cblhs";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".opencontainers.go-digest
  ];
}
