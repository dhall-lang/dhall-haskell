{ mkDerivation, base, ghc-prim, stdenv, transformers }:
mkDerivation {
  pname = "primitive";
  version = "0.6.4.0";
  sha256 = "4cbeaf7924dd79221f327ea101a29bf35c4976dc3319df157ff46ea68e6a0c64";
  revision = "1";
  editedCabalFile = "18a14k1yiam1m4l29rin9a0y53yp3nxvkz358nysld8aqwy2qsjv";
  libraryHaskellDepends = [ base ghc-prim transformers ];
  homepage = "https://github.com/haskell/primitive";
  description = "Primitive memory-related operations";
  license = stdenv.lib.licenses.bsd3;
}
