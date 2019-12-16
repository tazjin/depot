{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/docker/go-units";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "docker";
    repo = "go-units";
    rev = "519db1ee28dcc9fd2474ae59fca29a810482bfb1";
    sha256 = "0k8gja8ql4pqg5rzmqvka42vjfs6rzablak87whcnqba6qxpimvz";
  };
}
