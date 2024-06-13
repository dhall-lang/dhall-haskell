{ mkDerivation, aeson, base, binary, containers, data-default
, deepseq, Diff, directory, dlist, exceptions, file-embed, filepath
, generic-arbitrary, hashable, hspec, hspec-discover
, indexed-traversable, indexed-traversable-instances, lens
, lens-aeson, lib, mod, mtl, network-uri, prettyprinter, QuickCheck
, quickcheck-instances, regex, row-types, safe, some
, template-haskell, text
}:
mkDerivation {
  pname = "lsp-types";
  version = "2.1.1.0";
  sha256 = "409b0831a27f0c579f2ef792ae14ae5603c5921f8334826d3b5bb91ec206593f";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary containers data-default deepseq Diff dlist
    exceptions file-embed filepath generic-arbitrary hashable
    indexed-traversable indexed-traversable-instances lens lens-aeson
    mod mtl network-uri prettyprinter QuickCheck quickcheck-instances
    row-types safe some template-haskell text
  ];
  executableHaskellDepends = [
    base containers directory filepath mtl prettyprinter regex text
  ];
  testHaskellDepends = [
    aeson base filepath hspec lens network-uri QuickCheck
    quickcheck-instances row-types text
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  homepage = "https://github.com/haskell/lsp";
  description = "Haskell library for the Microsoft Language Server Protocol, data types";
  license = lib.licenses.mit;
  mainProgram = "generator";
}
