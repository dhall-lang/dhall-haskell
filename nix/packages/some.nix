{ mkDerivation, base, deepseq, stdenv }:
mkDerivation {
  pname = "some";
  version = "1.0.1";
  sha256 = "31ce29bf7d4849d6852d2f8947de0302d18fcfb95e6d01118b5533c975dfb78d";
  revision = "1";
  editedCabalFile = "0gpr24rf427l82d8gb3x97yj03vc2v8ky3b6m1gb4j3g4yvvmr96";
  libraryHaskellDepends = [ base deepseq ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/phadej/some";
  description = "Existential type: Some";
  license = stdenv.lib.licenses.bsd3;
}
