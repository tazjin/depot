{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/docker/distribution";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "docker";
    repo = "distribution";
    rev = "a8371794149d1d95f1e846744b05c87f2f825e5a";
    sha256 = "159rdbawff5cnhlizhhmwaj7g71m7ffc8n6il0ns7mndk6r3s961";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".opencontainers.go-digest
  ];
}
