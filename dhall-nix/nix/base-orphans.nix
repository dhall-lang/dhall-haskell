{ mkDerivation, base, ghc-prim, hspec, hspec-discover, QuickCheck
, stdenv
}:
mkDerivation {
  pname = "base-orphans";
  version = "0.8.1";
  sha256 = "442bd63aed102e753b2fed15df8ae19f35ee07af26590da63837c523b64a99db";
  libraryHaskellDepends = [ base ghc-prim ];
  testHaskellDepends = [ base hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell-compat/base-orphans#readme";
  description = "Backwards-compatible orphan instances for base";
  license = stdenv.lib.licenses.mit;
}
