{ mkDerivation, base, StateVar, stdenv, transformers }:
mkDerivation {
  pname = "contravariant";
  version = "1.5";
  sha256 = "6ef067b692ad69ffff294b953aa85f3ded459d4ae133c37896222a09280fc3c2";
  libraryHaskellDepends = [ base StateVar transformers ];
  homepage = "http://github.com/ekmett/contravariant/";
  description = "Contravariant functors";
  license = stdenv.lib.licenses.bsd3;
}
