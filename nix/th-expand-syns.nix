{ mkDerivation, base, containers, stdenv, syb, template-haskell }:
mkDerivation {
  pname = "th-expand-syns";
  version = "0.4.4.0";
  sha256 = "cc0f52d1364ace9ba56f51afd9106a5fe01ed3f5ae45c958c1b0f83be0a6f906";
  revision = "1";
  editedCabalFile = "1zbdg3hrqv7rzlsrw4a2vjr3g4nzny32wvjcpxamlvx77b1jvsw9";
  libraryHaskellDepends = [ base containers syb template-haskell ];
  testHaskellDepends = [ base template-haskell ];
  homepage = "https://github.com/DanielSchuessler/th-expand-syns";
  description = "Expands type synonyms in Template Haskell ASTs";
  license = stdenv.lib.licenses.bsd3;
}
