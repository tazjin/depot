{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/gogo/protobuf";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "gogo";
    repo = "protobuf";
    rev = "5628607bb4c51c3157aacc3a50f0ab707582b805";
    sha256 = "0x77x64sxjgfhmbijqfzmj8h4ar25l2w97h01q3cqs1wk7zfnkhp";
  };
}
