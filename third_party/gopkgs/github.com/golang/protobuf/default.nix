{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/golang/protobuf";
  src = builtins.fetchGit {
    url = "https://github.com/golang/protobuf";
    rev = "ed6926b37a637426117ccab59282c3839528a700";
  };

  deps = with pkgs.third_party; [
  ];
}
