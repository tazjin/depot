{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/golang/protobuf";
  src = builtins.fetchGit {
    url = "https://github.com/golang/protobuf";
    rev = "4c88cc3f1a34ffade77b79abc53335d1e511f25b";
  };

  deps = with pkgs.third_party; [
  ];
}
