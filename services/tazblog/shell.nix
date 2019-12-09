{ pkgs ? (import ../../default.nix {}).third_party.nixpkgs }:

let tazblog = import ./tazblog.nix;
    depNames = with builtins; filter (
      p: hasAttr p pkgs.haskellPackages
    ) (attrNames (functionArgs tazblog));
    ghc = pkgs.ghc.withPackages(p: map (x: p."${x}") depNames);
in pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = [ ghc pkgs.hlint ];
}
