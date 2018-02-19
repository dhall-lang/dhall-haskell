{ mkDerivation, ansi-wl-pprint, base, bytestring, containers
, criterion, deepseq, doctest, mtl, pgp-wordlist, QuickCheck
, random, stdenv, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, text, transformers
}:
mkDerivation {
  pname = "prettyprinter";
  version = "1.2.0.1";
  sha256 = "11397b182138efc8f7b09a70873093fb565d070e4c8f92cdde9e601bcd5a0566";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base text ];
  executableHaskellDepends = [ base template-haskell text ];
  testHaskellDepends = [
    base bytestring doctest pgp-wordlist tasty tasty-hunit
    tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [
    ansi-wl-pprint base containers criterion deepseq mtl QuickCheck
    random text transformers
  ];
  homepage = "http://github.com/quchen/prettyprinter";
  description = "A modern, easy to use, well-documented, extensible prettyprinter";
  license = stdenv.lib.licenses.bsd2;
}
