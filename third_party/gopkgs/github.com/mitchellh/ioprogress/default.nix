{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/mitchellh/ioprogress";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "mitchellh";
    repo = "ioprogress";
    rev = "6a23b12fa88ea53e63d9b49f148fe6b29c30af6d";
    sha256 = "1fcfwi5fzv17iahif42y7dhjfnjwkslk03zb9cniw42wyiwhj3jk";
  };
}
