{ pkgs, ... }:

pkgs.buildGo.external {
  path = "golang.org/x/sync";
  src = builtins.fetchGit {
    url = "https://go.googlesource.com/sync";
    rev = "cd5d95a43a6e21273425c7ae415d3df9ea832eeb";
  };
}
