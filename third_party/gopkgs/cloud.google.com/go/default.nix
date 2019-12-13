{ pkgs, ... }:

pkgs.buildGo.external {
  path = "cloud.google.com/go";
  src = builtins.fetchGit {
    url = "https://code.googlesource.com/gocloud";
    rev = "4f03f8e4ba168c636e1c218da7ab41a1c8c0d8cf";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
  ];
}
