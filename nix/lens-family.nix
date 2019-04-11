{ mkDerivation, base, containers, lens-family-core, mtl, stdenv
, transformers
}:
mkDerivation {
  pname = "lens-family";
  version = "1.2.3";
  sha256 = "8059e2b7a917e0108861ca795b0adfbb0bf1db5b1bdb55e677256a37d8de0e29";
  libraryHaskellDepends = [
    base containers lens-family-core mtl transformers
  ];
  description = "Lens Families";
  license = stdenv.lib.licenses.bsd3;
}
