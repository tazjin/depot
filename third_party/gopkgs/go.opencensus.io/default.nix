{ pkgs, ... }:

pkgs.buildGo.external {
  path = "go.opencensus.io";
  src = builtins.fetchGit {
    url = "https://github.com/census-instrumentation/opencensus-go";
    rev = "b4a14686f0a98096416fe1b4cb848e384fb2b22b";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".hashicorp.golang-lru.simplelru
  ];
}
