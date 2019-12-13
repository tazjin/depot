{ pkgs, ... }:

pkgs.buildGo.external {
  path = "go.opencensus.io";
  src = builtins.fetchGit {
    url = "https://github.com/census-instrumentation/opencensus-go";
    rev = "643eada29081047b355cfaa1ceb9bc307a10423c";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".hashicorp.golang-lru.simplelru
    gopkgs."github.com".golang.groupcache.lru
  ];
}
