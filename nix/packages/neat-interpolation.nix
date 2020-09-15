{ mkDerivation, base, megaparsec, QuickCheck, quickcheck-instances
, rerebase, stdenv, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, text
}:
mkDerivation {
  pname = "neat-interpolation";
  version = "0.5.1.2";
  sha256 = "962a4a92da4911c8e5b784ed43200b764ea8c6b3add032a09c57658e4b4684a1";
  libraryHaskellDepends = [ base megaparsec template-haskell text ];
  testHaskellDepends = [
    QuickCheck quickcheck-instances rerebase tasty tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/nikita-volkov/neat-interpolation";
  description = "A quasiquoter for neat and simple multiline text interpolation";
  license = stdenv.lib.licenses.mit;
}
