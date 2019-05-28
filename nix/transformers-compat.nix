{ mkDerivation, base, ghc-prim, stdenv, transformers }:
mkDerivation {
  pname = "transformers-compat";
  version = "0.6.4";
  sha256 = "ad7fb455d636533ba37fcae7b01666ebca07f668f3bd773d754b76f82d3ece0c";
  libraryHaskellDepends = [ base ghc-prim transformers ];
  homepage = "http://github.com/ekmett/transformers-compat/";
  description = "A small compatibility shim for the transformers library";
  license = stdenv.lib.licenses.bsd3;
}
