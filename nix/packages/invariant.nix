{ mkDerivation, array, base, bifunctors, comonad, containers
, contravariant, ghc-prim, hspec, hspec-discover, profunctors
, QuickCheck, StateVar, stdenv, stm, tagged, template-haskell
, th-abstraction, transformers, transformers-compat
, unordered-containers
}:
mkDerivation {
  pname = "invariant";
  version = "0.5.3";
  sha256 = "d73e5def38da9fdd85def073857aa5f4b1d3b0c2df05c43d58a677cca02d440c";
  revision = "1";
  editedCabalFile = "0sjy375npw6lqcckhyicizzv91v8jh3ca5yjkygiaj22bw5k8c74";
  libraryHaskellDepends = [
    array base bifunctors comonad containers contravariant ghc-prim
    profunctors StateVar stm tagged template-haskell th-abstraction
    transformers transformers-compat unordered-containers
  ];
  testHaskellDepends = [ base hspec QuickCheck template-haskell ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/nfrisby/invariant-functors";
  description = "Haskell98 invariant functors";
  license = stdenv.lib.licenses.bsd2;
}
