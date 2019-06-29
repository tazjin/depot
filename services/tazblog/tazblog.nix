{ mkDerivation, acid-state, base, base64-bytestring, blaze-html
, blaze-markup, bytestring, crypto-api, cryptohash, hamlet
, happstack-server, ixset, markdown, mtl, network, network-uri
, old-locale, options, rss, safecopy, shakespeare, stdenv, text
, time, transformers
}:
mkDerivation {
  pname = "tazblog";
  version = "5.1.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    acid-state base base64-bytestring blaze-html blaze-markup
    bytestring crypto-api cryptohash hamlet happstack-server ixset
    markdown mtl network network-uri old-locale rss safecopy
    shakespeare text time transformers
  ];
  executableHaskellDepends = [ acid-state base network options ];
  description = "Tazjin's Blog";
  license = stdenv.lib.licenses.mit;
}
