{ mkDerivation, base, deepseq, QuickCheck, stdenv }:
mkDerivation {
  pname = "generic-random";
  version = "1.3.0.0";
  sha256 = "f3c3025f5d9a44252a26d67446e2077331c005217233a1b52abec90fd6a6c2fc";
  libraryHaskellDepends = [ base QuickCheck ];
  testHaskellDepends = [ base deepseq QuickCheck ];
  homepage = "http://github.com/lysxia/generic-random";
  description = "Generic random generators";
  license = stdenv.lib.licenses.mit;
}
