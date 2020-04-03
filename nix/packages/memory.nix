{ mkDerivation, base, basement, bytestring, deepseq, foundation
, ghc-prim, stdenv
}:
mkDerivation {
  pname = "memory";
  version = "0.14.18";
  sha256 = "f5458d170a291788ac8da896bb44b0cc84021c99dd596c52adf2f7a7f6c03507";
  revision = "1";
  editedCabalFile = "0h4d0avv8kv3my4rim79lcamv2dyibld7w6ianq46nhwgr0h2lzm";
  libraryHaskellDepends = [
    base basement bytestring deepseq ghc-prim
  ];
  testHaskellDepends = [ base basement bytestring foundation ];
  homepage = "https://github.com/vincenthz/hs-memory";
  description = "memory and related abstraction stuff";
  license = stdenv.lib.licenses.bsd3;
}
