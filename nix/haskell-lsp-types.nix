{ mkDerivation, aeson, base, bytestring, data-default, deepseq
, filepath, hashable, lens, network-uri, scientific, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "haskell-lsp-types";
  version = "0.19.0.0";
  sha256 = "a942ab1e504d8ae61b586c9b048e3b1422ab793bd908062332180af01beb3921";
  libraryHaskellDepends = [
    aeson base bytestring data-default deepseq filepath hashable lens
    network-uri scientific text unordered-containers
  ];
  homepage = "https://github.com/alanz/haskell-lsp";
  description = "Haskell library for the Microsoft Language Server Protocol, data types";
  license = stdenv.lib.licenses.mit;
}
