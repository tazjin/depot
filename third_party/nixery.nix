# Technically I suppose Nixery is not a third-party program, but it's
# outside of this repository ...
{ pkgs }:

let src = pkgs.fetchFromGitHub {
  owner = "google";
  repo = "nixery";
  rev = "811a1fd228906c89af61d340be8f44788152dad9";
  sha256 = "0jsps86kr64rbnyi49w4pv9pr2fz1wp80zzk5a8ap6q21daky4q0";
};
in import src {
  inherit pkgs;
  preLaunch = ''
    export USER=root
    cachix use tazjin
  '';
  extraPackages = with pkgs; [ cachix ];
}
