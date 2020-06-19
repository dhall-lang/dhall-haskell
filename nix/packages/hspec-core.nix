{ mkDerivation, ansi-terminal, array, base, call-stack, clock
, deepseq, directory, filepath, hspec-expectations, hspec-meta
, HUnit, process, QuickCheck, quickcheck-io, random, setenv
, silently, stdenv, stm, temporary, tf-random, transformers
}:
mkDerivation {
  pname = "hspec-core";
  version = "2.7.1";
  sha256 = "2ccc20f27970f753ed2e902c323f4562adaf1a31f4234b3504e02a8a50417323";
  revision = "1";
  editedCabalFile = "0aw68sgz2p63y0vg07c1jx2pr8lmhp5c1ck60dlipyxsa00455i6";
  libraryHaskellDepends = [
    ansi-terminal array base call-stack clock deepseq directory
    filepath hspec-expectations HUnit QuickCheck quickcheck-io random
    setenv stm tf-random transformers
  ];
  testHaskellDepends = [
    ansi-terminal array base call-stack clock deepseq directory
    filepath hspec-expectations hspec-meta HUnit process QuickCheck
    quickcheck-io random setenv silently stm temporary tf-random
    transformers
  ];
  testToolDepends = [ hspec-meta ];
  testTarget = "--test-option=--skip --test-option='Test.Hspec.Core.Runner.hspecResult runs specs in parallel'";
  homepage = "http://hspec.github.io/";
  description = "A Testing Framework for Haskell";
  license = stdenv.lib.licenses.mit;
}
