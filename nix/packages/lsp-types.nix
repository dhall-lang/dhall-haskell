{ mkDerivation, aeson, base, binary, containers, data-default
, deepseq, Diff, dlist, exceptions, filepath, hashable, hspec
, hspec-discover, lens, lib, mod, mtl, network-uri, QuickCheck
, quickcheck-instances, safe, scientific, some, template-haskell
, text, tuple, unordered-containers
}:
mkDerivation {
  pname = "lsp-types";
  version = "1.6.0.1";
  sha256 = "47d084f3a56195706381b51ad2f5d1b22a958238f3e5519625dbc54a2503f670";
  libraryHaskellDepends = [
    aeson base binary containers data-default deepseq Diff dlist
    exceptions filepath hashable lens mod mtl network-uri safe
    scientific some template-haskell text unordered-containers
  ];
  testHaskellDepends = [
    aeson base filepath hspec lens network-uri QuickCheck
    quickcheck-instances text tuple
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell/lsp";
  description = "Haskell library for the Microsoft Language Server Protocol, data types";
  license = lib.licenses.mit;
}
