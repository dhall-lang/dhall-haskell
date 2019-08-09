{ mkDerivation, aeson, base, bytestring, data-default, deepseq
, filepath, hashable, lens, network-uri, scientific, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "haskell-lsp-types";
  version = "0.15.0.0";
  sha256 = "6b21b7de49d02df51ab9a25a0cf514b47b76ac3c8e46f3079431b6f52e05c2cf";
  libraryHaskellDepends = [
    aeson base bytestring data-default deepseq filepath hashable lens
    network-uri scientific text unordered-containers
  ];
  homepage = "https://github.com/alanz/haskell-lsp";
  description = "Haskell library for the Microsoft Language Server Protocol, data types";
  license = stdenv.lib.licenses.mit;
}
