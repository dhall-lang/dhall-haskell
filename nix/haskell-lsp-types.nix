{ mkDerivation, aeson, base, bytestring, data-default, deepseq
, filepath, hashable, lens, network-uri, scientific, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "haskell-lsp-types";
  version = "0.17.0.0";
  sha256 = "77444fa262393ac58b72b5cb6a4b1db401cdea015b42cab427bb4681dcd1230e";
  libraryHaskellDepends = [
    aeson base bytestring data-default deepseq filepath hashable lens
    network-uri scientific text unordered-containers
  ];
  homepage = "https://github.com/alanz/haskell-lsp";
  description = "Haskell library for the Microsoft Language Server Protocol, data types";
  license = stdenv.lib.licenses.mit;
}
