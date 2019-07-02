{ pkgs ? import <nixpkgs> {} }:

with pkgs; stdenv.mkDerivation rec {
  name = "gemma";

  src = fetchFromGitHub {
    owner  = "tazjin";
    repo   = "gemma";
    rev    = "61be253d6baa99f0a2208425b8a03b444bb1b184";
    sha256 = "0vbmz2aphcida728rc0z3k7gychs4w1778vsjbrs0ljk9qgbmyr5";
  };

  buildInputs = with lispPackages; [
    sbcl
    quicklisp
    hunchentoot
    cl-json
    local-time
    elmPackages.elm
    pkgconfig
  ];

  # The build phase has three distinct things it needs to do:
  #
  # 1. "Compile" the Elm source into something useful to browsers.
  #
  # 2. Configure the Lisp part of the application to serve the compiled Elm
  #
  # 3. Build (and don't strip!) an executable out of the Lisp backend.
  buildPhase = ''
    mkdir -p $out/share/gemma $out/bin
    mkdir .home && export HOME="$PWD/.home"

    # Build Elm
    cd frontend
    elm-make --yes Main.elm --output $out/share/gemma/index.html

    # Build Lisp
    cd $src
    quicklisp init
    env GEMMA_BIN_TARGET=$out/bin/gemma sbcl --load build.lisp
  '';

  installPhase = "true";

  # Stripping an SBCL executable removes the application, which is unfortunate.
  dontStrip = true;

  meta = with stdenv.lib; {
    description = "Tool for tracking recurring tasks";
    homepage    = "https://github.com/tazjin/gemma";
    license     = licenses.gpl3;
  };
}
