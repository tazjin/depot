# Technically I suppose Nixery is not a third-party program, but it's
# outside of this repository ...
{ pkgs }:

let src = pkgs.fetchFromGitHub {
  owner = "google";
  repo = "nixery";
  rev = "5d64be1969f397236883f140b4d533497622387c";
  sha256 = "0vxng5iy6m5a9b38x1mama6idagdinlv001fdkmjgj25sgyrqy2h";
};
in import src {
  inherit pkgs;
  preLaunch = ''
    export USER=root
    cachix use tazjin
  '';
  extraPackages = with pkgs; [ cachix iana-etc ];
}
