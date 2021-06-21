{ mkDerivation, base, bytestring, containers, deepseq, directory
, hashable, hedgehog, hspec, hspec-golden, hspec-hedgehog
, hspec-megaparsec, lib, markdown-unlit, megaparsec, mtl
, parser-combinators, text, time, transformers
, unordered-containers, validation-selective
}:
mkDerivation {
  pname = "tomland";
  version = "1.3.2.0";
  sha256 = "cc4e959be89b368f7e1619bf53c73bc56cfa32f543a3113396638f4f604d437a";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers deepseq hashable megaparsec mtl
    parser-combinators text time transformers unordered-containers
    validation-selective
  ];
  executableHaskellDepends = [
    base bytestring containers hashable text time unordered-containers
  ];
  executableToolDepends = [ markdown-unlit ];
  testHaskellDepends = [
    base bytestring containers directory hashable hedgehog hspec
    hspec-golden hspec-hedgehog hspec-megaparsec megaparsec text time
    unordered-containers
  ];
  homepage = "https://github.com/kowainik/tomland";
  description = "Bidirectional TOML serialization";
  license = lib.licenses.mpl20;
}
