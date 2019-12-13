{ pkgs, ... }:

pkgs.buildGo.external {
  path = "golang.org/x/text";
  src = builtins.fetchGit {
    url = "https://go.googlesource.com/text";
    rev = "342b2e1fbaa52c93f31447ad2c6abc048c63e475";
  };

  deps = with pkgs.third_party; [
  ];
}
