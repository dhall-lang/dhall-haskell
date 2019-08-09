{ mkDerivation, aeson, async, attoparsec, base, bytestring
, containers, data-default, directory, filepath, hashable
, haskell-lsp-types, hslogger, hspec, hspec-discover, lens, mtl
, network-uri, QuickCheck, quickcheck-instances, rope-utf16-splay
, sorted-list, stdenv, stm, temporary, text, time
, unordered-containers
}:
mkDerivation {
  pname = "haskell-lsp";
  version = "0.15.0.0";
  sha256 = "8fddcc317494a308a115abc8789581db301f3fd7906e356e3a49da4b1b042c84";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base bytestring containers data-default
    directory filepath hashable haskell-lsp-types hslogger lens mtl
    network-uri rope-utf16-splay sorted-list stm temporary text time
    unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring containers data-default directory filepath
    hashable hspec lens network-uri QuickCheck quickcheck-instances
    rope-utf16-splay sorted-list stm text
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/alanz/haskell-lsp";
  description = "Haskell library for the Microsoft Language Server Protocol";
  license = stdenv.lib.licenses.mit;
}
