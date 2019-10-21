{ mkDerivation, ansi-wl-pprint, base, base-compat, bytestring
, containers, criterion, deepseq, doctest, mtl, pgp-wordlist
, QuickCheck, random, stdenv, tasty, tasty-hunit, tasty-quickcheck
, text, transformers
}:
mkDerivation {
  pname = "prettyprinter";
  version = "1.4.0";
  sha256 = "7f1d9224f9e577eb24dda695beb6bc2f074e93a84a3c9f11bb578aa6ed39cb45";
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
