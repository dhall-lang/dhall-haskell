{ mkDerivation, base, base-compat, base-orphans, containers
, ghc-boot-th, ghc-prim, hspec, hspec-discover, QuickCheck, stdenv
, tagged, template-haskell, th-abstraction, transformers
, transformers-compat
}:
mkDerivation {
  pname = "deriving-compat";
  version = "0.5.8";
  sha256 = "39ddf88affc68dbed3b6835c270d75bee44a667237c50fc62f2371c0afe7f6b7";
  libraryHaskellDepends = [
    base containers ghc-boot-th ghc-prim template-haskell
    th-abstraction transformers transformers-compat
  ];
  testHaskellDepends = [
    base base-compat base-orphans hspec QuickCheck tagged
    template-haskell transformers transformers-compat
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell-compat/deriving-compat";
  description = "Backports of GHC deriving extensions";
  license = stdenv.lib.licenses.bsd3;
}
