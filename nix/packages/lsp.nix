{ mkDerivation, aeson, async, attoparsec, base, bytestring
, co-log-core, containers, data-default, directory, exceptions
, filepath, hashable, hspec, hspec-discover, lens, lib, lsp-types
, mtl, prettyprinter, random, sorted-list, stm, temporary, text
, text-rope, transformers, unliftio-core, unordered-containers
, uuid
}:
mkDerivation {
  pname = "lsp";
  version = "1.6.0.0";
  sha256 = "896803766e8ceabeacc72743f4b92cf7766b2a1f09be270b29d0a39692b00470";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base bytestring co-log-core containers
    data-default directory exceptions filepath hashable lens lsp-types
    mtl prettyprinter random sorted-list stm temporary text text-rope
    transformers unliftio-core unordered-containers uuid
  ];
  testHaskellDepends = [
    base containers hspec sorted-list text text-rope
    unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell/lsp";
  description = "Haskell library for the Microsoft Language Server Protocol";
  license = lib.licenses.mit;
}
