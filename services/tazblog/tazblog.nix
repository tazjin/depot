{ mkDerivation, acid-state, base, base64-bytestring, blaze-html
, blaze-markup, bytestring, crypto-api, cryptohash, hamlet
, happstack-server, ixset, markdown, mtl, network, network-uri
, old-locale, options, rss, safecopy, shakespeare, stdenv, text
, time, transformers
}:
mkDerivation {
  pname = "tazblog";
  version = "6.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base64-bytestring blaze-html blaze-markup bytestring
    crypto-api cryptohash hamlet happstack-server markdown mtl
    network network-uri old-locale rss shakespeare text time
    transformers
  ];
  executableHaskellDepends = [ base network options ];
  description = "Tazjin's Blog";
  license = stdenv.lib.licenses.mit;
}
