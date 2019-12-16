{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/docker/docker-credential-helpers";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "docker";
    repo = "docker-credential-helpers";
    rev = "f78081d1f7fef6ad74ad6b79368de6348386e591";
    sha256 = "1gzvvzis2ba8d5zl4hiarnplcyr11jag2xyn1jp50daqigkiymf8";
  };
}
