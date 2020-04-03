{ mkDerivation, base, containers, deepseq, erf, process, random
, stdenv, template-haskell, tf-random, transformers
}:
mkDerivation {
  pname = "QuickCheck";
  version = "2.12.6.1";
  sha256 = "0b2aa7f5c625b5875c36f5f548926fcdaedf4311bd3a4c291fcf10b8d7faa170";
  revision = "1";
  editedCabalFile = "0w5gygp6pmyjzjjx5irfflcbx586zfnqidq669ssqqfsadf944xv";
  libraryHaskellDepends = [
    base containers deepseq erf random template-haskell tf-random
    transformers
  ];
  testHaskellDepends = [ base deepseq process ];
  homepage = "https://github.com/nick8325/quickcheck";
  description = "Automatic testing of Haskell programs";
  license = stdenv.lib.licenses.bsd3;
}
