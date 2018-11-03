{ mkDerivation, base, ghc-prim, hspec, hspec-discover, QuickCheck
, stdenv
}:
mkDerivation {
  pname = "base-orphans";
  version = "0.8";
  sha256 = "aceec656bfb4222ad3035c3d87d80130b42b595b72888f9ab59c6dbb7ed24817";
  libraryHaskellDepends = [ base ghc-prim ];
  testHaskellDepends = [ base hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell-compat/base-orphans#readme";
  description = "Backwards-compatible orphan instances for base";
  license = stdenv.lib.licenses.mit;
}
