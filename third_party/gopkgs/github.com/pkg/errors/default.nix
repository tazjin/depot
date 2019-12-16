{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/pkg/errors";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "pkg";
    repo = "errors";
    rev = "7f95ac13edff643b8ce5398b6ccab125f8a20c1a";
    sha256 = "01hfgyxiciqzd9dal7sdskl86a6kpd1flkr7f6v9n6a3xlvlm9s3";
  };
}
