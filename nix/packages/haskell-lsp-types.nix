{ mkDerivation, aeson, base, binary, bytestring, data-default
, deepseq, filepath, hashable, lens, network-uri, scientific
, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "haskell-lsp-types";
  version = "0.22.0.0";
  sha256 = "756a78674b023d19d5a900bddb1c9464b4ff8fb68aa2804181ba4e9e4b2b8714";
  libraryHaskellDepends = [
    aeson base binary bytestring data-default deepseq filepath hashable
    lens network-uri scientific text unordered-containers
  ];
  homepage = "https://github.com/alanz/haskell-lsp";
  description = "Haskell library for the Microsoft Language Server Protocol, data types";
  license = stdenv.lib.licenses.mit;
}
