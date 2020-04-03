{ mkDerivation, aeson, base, binary, bytestring, data-default
, deepseq, filepath, hashable, lens, network-uri, scientific
, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "haskell-lsp-types";
  version = "0.21.0.0";
  sha256 = "22c5cd0a37ddee873146b78cfaa6b8c40f01adb9bf6e6e4e063901bf9385bd74";
  libraryHaskellDepends = [
    aeson base binary bytestring data-default deepseq filepath hashable
    lens network-uri scientific text unordered-containers
  ];
  homepage = "https://github.com/alanz/haskell-lsp";
  description = "Haskell library for the Microsoft Language Server Protocol, data types";
  license = stdenv.lib.licenses.mit;
}
