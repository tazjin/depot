{ pkgs, ... }:

pkgs.buildGo.external {
  path = "golang.org/x/sys";
  src = builtins.fetchGit {
    url = "https://go.googlesource.com/sys";
    rev = "fde4db37ae7ad8191b03d30d27f258b5291ae4e3";
  };

  deps = with pkgs.third_party; [
  ];
}
