{ mkDerivation, aeson, base, bytestring, deepseq, exceptions
, filepath, genvalidity, genvalidity-hspec, genvalidity-property
, hashable, hspec, mtl, QuickCheck, stdenv, template-haskell, text
, validity
}:
mkDerivation {
  pname = "path";
  version = "0.8.0";
  sha256 = "24cb49bb585f33a3b334ab55cb9bac251b66afdb617b71a20dbaeb820351fa6f";
  libraryHaskellDepends = [
    aeson base deepseq exceptions filepath hashable template-haskell
    text
  ];
  testHaskellDepends = [
    aeson base bytestring filepath genvalidity genvalidity-hspec
    genvalidity-property hspec mtl QuickCheck template-haskell validity
  ];
  description = "Support for well-typed paths";
  license = stdenv.lib.licenses.bsd3;
}
