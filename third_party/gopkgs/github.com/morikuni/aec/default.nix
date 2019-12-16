{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/morikuni/aec";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "morikuni";
    repo = "aec";
    rev = "39771216ff4c63d11f5e604076f9c45e8be1067b";
    sha256 = "1qaqh0lk9wrqgff0yrxnbznvmwyhdxy3g9b2hjpazp5bw4nj0dp7";
  };
}
