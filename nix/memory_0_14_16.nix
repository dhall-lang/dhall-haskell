{ mkDerivation, base, basement, bytestring, deepseq, foundation
, ghc-prim, stdenv
}:
mkDerivation {
  pname = "memory";
  version = "0.14.16";
  sha256 = "7bb0834ab28ce1248f3be09df211d49d20d703cdcda3ed16cde99356e2d72b0f";
  revision = "1";
  editedCabalFile = "10j8737fm287ii0nm4hqnhf87apls3xjczkzdw9qqkb4a2dybsbx";
  libraryHaskellDepends = [
    base basement bytestring deepseq foundation ghc-prim
  ];
  testHaskellDepends = [ base basement bytestring foundation ];
  homepage = "https://github.com/vincenthz/hs-memory";
  description = "memory and related abstraction stuff";
  license = stdenv.lib.licenses.bsd3;
}
