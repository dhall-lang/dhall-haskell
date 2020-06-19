{ mkDerivation, base, containers, lens-family-core, mtl, stdenv
, transformers
}:
mkDerivation {
  pname = "lens-family";
  version = "1.2.1";
  sha256 = "41838eba3fa063dc467bbcd70f9395c8429c08ecab2ff779d01b459222cd9ab7";
  libraryHaskellDepends = [
    base containers lens-family-core mtl transformers
  ];
  description = "Lens Families";
  license = stdenv.lib.licenses.bsd3;
}
