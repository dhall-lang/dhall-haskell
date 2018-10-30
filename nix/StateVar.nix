{ mkDerivation, base, stdenv, stm, transformers }:
mkDerivation {
  pname = "StateVar";
  version = "1.1.1.1";
  sha256 = "eb6436516ab2d5e3d3e070b5a1595c4dceea760a58a9cc8d23dad5f6008f2223";
  libraryHaskellDepends = [ base stm transformers ];
  homepage = "https://github.com/haskell-opengl/StateVar";
  description = "State variables";
  license = stdenv.lib.licenses.bsd3;
}
