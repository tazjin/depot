{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/golang/groupcache";
  src = builtins.fetchGit {
    url = "https://github.com/golang/groupcache";
    rev = "611e8accdfc92c4187d399e95ce826046d4c8d73";
  };
}
