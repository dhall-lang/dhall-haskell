{ mkDerivation, aeson, aeson-pretty, ansi-terminal, async, base
, bytestring, co-log-core, conduit, conduit-parse, containers
, data-default, Diff, directory, exceptions, extra, filepath, Glob
, hspec, lens, lib, lsp, lsp-types, mtl, parser-combinators
, process, row-types, some, text, time, transformers, unix
, unliftio
}:
mkDerivation {
  pname = "lsp-test";
  version = "0.15.0.1";
  sha256 = "ad5be9baa344337b87958dfeb765e3edceca47c4ada57fb1ffeccf4056c57ad8";
  libraryHaskellDepends = [
    aeson aeson-pretty ansi-terminal async base bytestring co-log-core
    conduit conduit-parse containers data-default Diff directory
    exceptions filepath Glob lens lsp lsp-types mtl parser-combinators
    process row-types some text time transformers unix
  ];
  testHaskellDepends = [
    aeson base co-log-core containers data-default directory filepath
    hspec lens lsp mtl parser-combinators process text unliftio
  ];
  testToolDepends = [ lsp ];
  benchmarkHaskellDepends = [ base extra lsp process ];
  homepage = "https://github.com/haskell/lsp/blob/master/lsp-test/README.md";
  description = "Functional test framework for LSP servers";
  license = lib.licenses.bsd3;
}
