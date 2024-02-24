{ mkDerivation, aeson, async, attoparsec, base, bytestring
, co-log-core, containers, data-default, directory, exceptions
, filepath, hashable, hspec, hspec-discover, lens, lens-aeson, lib
, lsp-types, mtl, prettyprinter, random, row-types, sorted-list
, stm, text, text-rope, transformers, unliftio-core
, unordered-containers, uuid
}:
mkDerivation {
  pname = "lsp";
  version = "2.3.0.0";
  sha256 = "e29866bea5c2482a1ec42a7c948d504685362f1ee1393c4336d7dbed2ae4bb4b";
  revision = "1";
  editedCabalFile = "15jx8x106lnv824yw6mip10gxjbgqww4557xfbyi9nvmgb83h7xj";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base bytestring co-log-core containers
    data-default directory exceptions filepath hashable lens lens-aeson
    lsp-types mtl prettyprinter random row-types sorted-list stm text
    text-rope transformers unliftio-core unordered-containers uuid
  ];
  testHaskellDepends = [
    base containers hspec row-types sorted-list text text-rope
    unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell/lsp";
  description = "Haskell library for the Microsoft Language Server Protocol";
  license = lib.licenses.mit;
}
