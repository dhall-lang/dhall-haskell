{ mkDerivation, base, bifunctors, stdenv, tagged }:
mkDerivation {
  pname = "assoc";
  version = "1.0.1";
  sha256 = "4000dea2fbc272ff5a15a0bf0fae1c29dea4b87b8fb4ccb8b07b8114ee2636d5";
  libraryHaskellDepends = [ base bifunctors tagged ];
  description = "swap and assoc: Symmetric and Semigroupy Bifunctors";
  license = stdenv.lib.licenses.bsd3;
}
