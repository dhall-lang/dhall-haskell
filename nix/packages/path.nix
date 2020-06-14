{ mkDerivation, aeson, base, bytestring, deepseq, exceptions
, filepath, genvalidity, genvalidity-hspec, genvalidity-property
, hashable, hspec, mtl, QuickCheck, stdenv, template-haskell, text
, validity
}:
mkDerivation {
  pname = "path";
  version = "0.7.0";
  sha256 = "b328d285381ce00607444c96cf6d1bf4aa30e8dcb8e82017e5a33636abf487b6";
  revision = "1";
  editedCabalFile = "0ph5qs50lm8ac58v8df0mmivqfilb1wz14568q06aws6gwj9qqpi";
  libraryHaskellDepends = [
    aeson base deepseq exceptions filepath hashable template-haskell
    text
  ];
  testHaskellDepends = [
    aeson base bytestring filepath genvalidity genvalidity-hspec
    genvalidity-property hspec mtl QuickCheck validity
  ];
  description = "Support for well-typed paths";
  license = stdenv.lib.licenses.bsd3;
}
