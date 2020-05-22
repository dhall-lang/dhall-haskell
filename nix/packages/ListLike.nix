{ mkDerivation, array, base, bytestring, containers, deepseq, dlist
, fmlist, HUnit, QuickCheck, random, semigroups, stdenv, text
, utf8-string, vector
}:
mkDerivation {
  pname = "ListLike";
  version = "4.7";
  sha256 = "5492ed7589d04510c1d50597baa22768c97d02107c89ced195ea77368eea0988";
  libraryHaskellDepends = [
    array base bytestring containers deepseq dlist fmlist semigroups
    text utf8-string vector
  ];
  testHaskellDepends = [
    array base bytestring containers dlist fmlist HUnit QuickCheck
    random semigroups text utf8-string vector
  ];
  homepage = "http://github.com/ddssff/listlike";
  description = "Generalized support for list-like structures";
  license = stdenv.lib.licenses.bsd3;
}
