# Telega is an Emacs client for Telegram. It requires a native server
# component to run correctly, which is built by this derivation.
{ pkgs, ... }:

with pkgs.third_party;

stdenv.mkDerivation {
  name = "telega";
  buildInputs = [ tdlib ];

  src = fetchFromGitHub {
    owner = "zevlg";
    repo = "telega.el";
    rev = "d532b16067cf24728a2aa03a7aeaebe2ceac7df4";
    sha256 = "1s2sd07sin9sy833wqprhbfk5j1d1s4azzvj6d8k68sxlgz8996m";
  } + "/server";

  installPhase = ''
    mkdir -p $out/bin
    mv telega-server $out/bin/
  '';
}
