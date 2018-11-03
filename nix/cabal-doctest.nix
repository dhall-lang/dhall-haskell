{ mkDerivation, base, Cabal, directory, filepath, stdenv }:
mkDerivation {
  pname = "cabal-doctest";
  version = "1.0.6";
  sha256 = "decaaa5a73eaabaf3c4f8c644bd7f6e3f428b6244e935c0cf105f75f9b24ed2d";
  revision = "2";
  editedCabalFile = "1kbiwqm4fxrsdpcqijdq98h8wzmxydcvxd03f1z8dliqzyqsbd60";
  libraryHaskellDepends = [ base Cabal directory filepath ];
  homepage = "https://github.com/phadej/cabal-doctest";
  description = "A Setup.hs helper for doctests running";
  license = stdenv.lib.licenses.bsd3;
}
