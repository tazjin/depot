with import <nixpkgs> {}; 
let
  gemma = import ./default.nix;
  entrypoint = writeScript "entrypoint.sh" ''
    #!${stdenv.shell}
    set -e
    exec ${gemma}/bin/gemma
  '';
in dockerTools.buildImage {
  name = "gemma";
  contents = gemma; # [ gemma ];
  config = {
    Entrypoint = [ entrypoint ];
    WorkingDir = "/data";
    Volumes = {
      "/data" = {};
    };
  };
}
