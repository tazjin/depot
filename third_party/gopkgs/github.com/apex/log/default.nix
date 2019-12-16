{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/apex/log";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "apex";
    repo = "log";
    rev = "baa5455d10123171ef1951381610c51ad618542a";
    sha256 = "157g78m239fsy0dmfamcsmwag6ys8yhdy509h9x5k4zknlbjybia";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".pkg.errors
  ];
}
