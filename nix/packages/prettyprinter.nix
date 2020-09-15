{ mkDerivation, ansi-wl-pprint, base, base-compat, bytestring
, containers, deepseq, doctest, gauge, mtl, pgp-wordlist
, QuickCheck, quickcheck-instances, random, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, transformers
}:
mkDerivation {
  pname = "prettyprinter";
  version = "1.7.0";
  sha256 = "591b87ce8a5cff39d66cb1c156c7d27d04de57952f16eb3ce3afe309ac26e0a7";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base text ];
  testHaskellDepends = [
    base bytestring doctest pgp-wordlist QuickCheck
    quickcheck-instances tasty tasty-hunit tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [
    ansi-wl-pprint base base-compat containers deepseq gauge mtl
    QuickCheck random text transformers
  ];
  homepage = "http://github.com/quchen/prettyprinter";
  description = "A modern, easy to use, well-documented, extensible pretty-printer";
  license = stdenv.lib.licenses.bsd2;
}
