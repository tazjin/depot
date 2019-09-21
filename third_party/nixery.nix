# Technically I suppose Nixery is not a third-party program, but it's
# outside of this repository ...
{ pkgs }:

let src = pkgs.fetchFromGitHub {
  owner = "google";
  repo = "nixery";
  rev = "924a4b2d3bf5ed9d7052d7ac09288bb0037daae1";
  sha256 = "1ccq2nk4kbprm48zzpvc132970wvdmvi1iyyrk8kbrf3b92gsm63";
};
in import src {
  inherit pkgs;
  preLaunch = ''
    export USER=root
    cachix use tazjin
  '';
  extraPackages = with pkgs; [ cachix ];
}
