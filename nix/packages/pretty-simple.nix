{ mkDerivation, aeson, ansi-terminal, base, bytestring, Cabal
, cabal-doctest, containers, criterion, doctest, Glob, mtl
, optparse-applicative, QuickCheck, stdenv, template-haskell, text
, transformers
}:
mkDerivation {
  pname = "pretty-simple";
  version = "3.1.0.0";
  sha256 = "ea8a062fe7a535a0778ea0f85442551b9a67fad95979fc3e4c617399452c775c";
  configureFlags = [ "-fbuildexample" "-fbuildexe" ];
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    ansi-terminal base containers mtl text transformers
  ];
  executableHaskellDepends = [
    aeson base bytestring optparse-applicative text
  ];
  testHaskellDepends = [
    base doctest Glob QuickCheck template-haskell
  ];
  benchmarkHaskellDepends = [ base criterion text ];
  homepage = "https://github.com/cdepillabout/pretty-simple";
  description = "pretty printer for data types with a 'Show' instance";
  license = stdenv.lib.licenses.bsd3;
}
