{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/Masterminds/semver";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "Masterminds";
    repo = "semver";
    rev = "910aa146bd66780c2815d652b92a7fc5331e533c";
    sha256 = "1b3956cdk0ps1vqjf0al05xzw8b61rvswk59qmjwqypkqp3a2n8c";
  };
}
