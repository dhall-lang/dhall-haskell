{ mkDerivation, base, containers, stdenv, transformers }:
mkDerivation {
  pname = "lens-family-core";
  version = "1.2.3";
  sha256 = "914f5f077d7bed8a93866ac696e69c35bb8d0fbe81314236288b057941703901";
  libraryHaskellDepends = [ base containers transformers ];
  description = "Haskell 98 Lens Families";
  license = stdenv.lib.licenses.bsd3;
}
