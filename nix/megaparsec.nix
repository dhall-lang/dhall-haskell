{ mkDerivation, base, bytestring, case-insensitive, containers
, criterion, deepseq, hspec, hspec-expectations, mtl
, parser-combinators, QuickCheck, scientific, stdenv, text
, transformers, weigh
}:
mkDerivation {
  pname = "megaparsec";
  version = "7.0.2";
  sha256 = "e888f6a1ef6c9908c9893f2cd4105d12d7778cf88f885b416915fcd89526c5db";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers deepseq mtl
    parser-combinators scientific text transformers
  ];
  testHaskellDepends = [
    base bytestring case-insensitive containers hspec
    hspec-expectations mtl parser-combinators QuickCheck scientific
    text transformers
  ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq text weigh
  ];
  homepage = "https://github.com/mrkkrp/megaparsec";
  description = "Monadic parser combinators";
  license = stdenv.lib.licenses.bsd2;
}
