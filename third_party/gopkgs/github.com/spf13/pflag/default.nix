{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/spf13/pflag";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "spf13";
    repo = "pflag";
    rev = "2e9d26c8c37aae03e3f9d4e90b7116f5accb7cab";
    sha256 = "0gpmacngd0gpslnbkzi263f5ishigzgh6pbdv9hp092rnjl4nd31";
  };
}
