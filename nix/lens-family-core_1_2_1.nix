{ mkDerivation, base, containers, stdenv, transformers }:
mkDerivation {
  pname = "lens-family-core";
  version = "1.2.1";
  sha256 = "95e3b9876a6cdcc6865bfad22e04af41430c7a9a6bc96e9a25a2a35a841d19a4";
  libraryHaskellDepends = [ base containers transformers ];
  description = "Haskell 98 Lens Families";
  license = stdenv.lib.licenses.bsd3;
}
