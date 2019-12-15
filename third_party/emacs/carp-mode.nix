{ pkgs, ... }:

with pkgs.third_party;
with emacsPackagesNg;

melpaBuild rec {
  pname = "carp-mode";
  version = "3.0";
  packageRequires = [ clojure-mode ];

  recipe = builtins.toFile "recipe" ''
    (carp-mode :fetcher github
        :repo "carp-lang/carp"
        :files ("emacs/*.el"))
  '';

  src = fetchFromGitHub {
    owner = "carp-lang";
    repo = "carp";
    rev = "6954642cadee730885717201c3180c7acfb1bfa9";
    sha256 = "1pz4x2qkwjbz789bwc6nkacrjpzlxawxhl2nv0xdp731y7q7xyk9";
  };
}
