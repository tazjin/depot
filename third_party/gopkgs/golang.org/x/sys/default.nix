{ pkgs, ... }:

pkgs.buildGo.external {
  path = "golang.org/x/sys";
  src = builtins.fetchGit {
    url = "https://go.googlesource.com/sys";
    rev = "ac6580df4449443a05718fd7858c1f91ad5f8d20";
  };
}
