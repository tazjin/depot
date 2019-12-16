{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/docker/cli";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "docker";
    repo = "cli";
    rev = "ebca1413117a3fcb81c89d6be226dcec74e5289f";
    sha256 = "07x8fpnw5m94v9432z86hv1p3b4cj2b8vlacfgb67mka845gvazm";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".docker.docker.pkg.homedir
    gopkgs."github.com".docker.docker-credential-helpers.client
    gopkgs."github.com".docker.docker-credential-helpers.credentials
    gopkgs."github.com".pkg.errors
  ];
}
