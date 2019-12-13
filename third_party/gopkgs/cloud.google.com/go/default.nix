{ pkgs, ... }:

pkgs.buildGo.external {
  path = "cloud.google.com/go";
  src = builtins.fetchGit {
    url = "https://code.googlesource.com/gocloud";
    rev = "76e973f7c1e722b4859698ace0daed4e7eccdc60";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
  ];
}
