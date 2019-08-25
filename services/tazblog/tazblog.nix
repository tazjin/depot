{ mkDerivation, aeson, acid-state, base, base64-bytestring, blaze-html
, bytestring, dns, happstack-server, markdown, network, network-uri, old-locale
, options, rss, shakespeare, stdenv, text, time }:
mkDerivation {
  pname = "tazblog";
  version = "6.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    base64-bytestring
    blaze-html
    bytestring
    dns
    happstack-server
    markdown
    network
    network-uri
    old-locale
    rss
    shakespeare
    text
    time
  ];
  executableHaskellDepends = [ base network options ];
  description = "Tazjin's Blog";
  license = stdenv.lib.licenses.mit;
}
