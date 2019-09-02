{ stdenv, sbcl, lispPackages, elmPackages, makeWrapper, openssl }:

let frontend = stdenv.mkDerivation {
  name = "gemma-frontend";
  src = ./frontend;
  buildInputs = [ elmPackages.elm ];

  phases = [ "unpackPhase" "buildPhase" ];
  buildPhase = ''
    mkdir .home && export HOME="$PWD/.home"
    mkdir -p $out
    ls -lh
    elm-make --yes Main.elm --output $out/index.html
  '';
};
in stdenv.mkDerivation rec {
  name = "gemma";
  src = ./.;

  nativeBuildInputs = with lispPackages; [
    sbcl
    hunchentoot
    cl-json
    cffi
    cl-prevalence
    local-time
    makeWrapper
  ];

  buildPhase = ''
    mkdir -p $out/share/gemma $out/bin

    # Build Lisp using the Nix-provided wrapper which sets the load
    # paths correctly.
    cd $src
    env GEMMA_BIN_TARGET=$out/bin/gemma common-lisp.sh --load build.lisp

    # Wrap gemma to find OpenSSL at runtime:
    wrapProgram $out/bin/gemma --prefix LD_LIBRARY_PATH : "${openssl.out}/lib"

    # and finally copy the frontend to the appropriate spot
    cp ${frontend}/index.html $out/share/gemma/index.html
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
