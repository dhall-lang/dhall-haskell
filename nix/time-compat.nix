{ mkDerivation, base, base-compat, base-orphans, deepseq, HUnit
, QuickCheck, stdenv, tagged, tasty, tasty-hunit, tasty-quickcheck
, time
}:
mkDerivation {
  pname = "time-compat";
  version = "1.9.2.2";
  sha256 = "a268613385d359274edf48fb2dad4af29874f58486b2d5625e3b95a371066a17";
  revision = "2";
  editedCabalFile = "1i94pch4zj713gxcrlb09airbh3hqqs7cscjjfkxk9lixkk6iwnc";
  libraryHaskellDepends = [ base base-orphans deepseq time ];
  testHaskellDepends = [
    base base-compat deepseq HUnit QuickCheck tagged tasty tasty-hunit
    tasty-quickcheck time
  ];
  homepage = "https://github.com/phadej/time-compat";
  description = "Compatibility package for time";
  license = stdenv.lib.licenses.bsd3;
}
