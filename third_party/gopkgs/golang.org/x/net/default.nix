{ pkgs, ... }:

pkgs.buildGo.external {
  path = "golang.org/x/net";
  src = builtins.fetchGit {
    url = "https://go.googlesource.com/net";
    rev = "74dc4d7220e7acc4e100824340f3e66577424772";
  };

  deps = with pkgs.third_party; [
    gopkgs."golang.org".x.text.secure.bidirule.gopkg
    gopkgs."golang.org".x.text.unicode.bidi.gopkg
    gopkgs."golang.org".x.text.unicode.norm.gopkg
  ];
}
