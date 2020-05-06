{ mkDerivation, base, megaparsec, QuickCheck, quickcheck-instances
, rerebase, stdenv, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, text
}:
mkDerivation {
  pname = "neat-interpolation";
  version = "0.5.1";
  sha256 = "5ffe87dcb058f1facf04e504aed70fcf872c3c1fd8c807d554bd0ee4c869de41";
  libraryHaskellDepends = [ base megaparsec template-haskell text ];
  testHaskellDepends = [
    QuickCheck quickcheck-instances rerebase tasty tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/nikita-volkov/neat-interpolation";
  description = "A quasiquoter for neat and simple multiline text interpolation";
  license = stdenv.lib.licenses.mit;
}
