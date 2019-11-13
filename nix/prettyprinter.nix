{ mkDerivation, ansi-wl-pprint, base, base-compat, bytestring
, containers, criterion, deepseq, doctest, mtl, pgp-wordlist
, QuickCheck, random, stdenv, tasty, tasty-hunit, tasty-quickcheck
, text, transformers
}:
mkDerivation {
  pname = "prettyprinter";
  version = "1.5.1";
  sha256 = "fb66b498cdd46aa7f36abdaf0b49e88444a3e6ed9d04bec8924ed6355f393794";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base text ];
  testHaskellDepends = [
    base bytestring doctest pgp-wordlist QuickCheck tasty tasty-hunit
    tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [
    ansi-wl-pprint base base-compat containers criterion deepseq mtl
    QuickCheck random text transformers
  ];
  homepage = "http://github.com/quchen/prettyprinter";
  description = "A modern, easy to use, well-documented, extensible pretty-printer";
  license = stdenv.lib.licenses.bsd2;
}
