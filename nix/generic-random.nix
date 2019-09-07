{ mkDerivation, base, deepseq, QuickCheck, stdenv }:
mkDerivation {
  pname = "generic-random";
  version = "1.2.0.0";
  sha256 = "9b1e00d2f06b582695a34cfdb2d8b62b32f64152c6ed43f5c2d776e6e9aa148c";
  revision = "1";
  editedCabalFile = "1d0hx41r7yq2a86ydnfh2fv540ah8cz05l071s2z4wxcjw0ymyn4";
  libraryHaskellDepends = [ base QuickCheck ];
  testHaskellDepends = [ base deepseq QuickCheck ];
  homepage = "http://github.com/lysxia/generic-random";
  description = "Generic random generators";
  license = stdenv.lib.licenses.mit;
}
