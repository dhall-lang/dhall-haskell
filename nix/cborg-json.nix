{ mkDerivation, aeson, aeson-pretty, base, cborg, scientific
, stdenv, text, unordered-containers, vector
}:
mkDerivation {
  pname = "cborg-json";
  version = "0.2.1.0";
  sha256 = "3fb6b54e6ddd322880689fb461f7911aca45b9758482c9f9949619c7d7b52006";
  libraryHaskellDepends = [
    aeson aeson-pretty base cborg scientific text unordered-containers
    vector
  ];
  homepage = "https://github.com/well-typed/cborg";
  description = "A library for encoding JSON as CBOR";
  license = stdenv.lib.licenses.bsd3;
}
