{ mkDerivation, aeson, aeson-pretty, ansi-terminal, async, base
, bytestring, co-log-core, conduit, conduit-parse, containers
, data-default, Diff, directory, exceptions, extra, filepath, Glob
, hspec, lens, lens-aeson, lib, lsp, lsp-types, mtl
, parser-combinators, process, row-types, some, text, time
, transformers, unix, unliftio
}:
mkDerivation {
  pname = "lsp-test";
  version = "0.16.0.1";
  sha256 = "607f12a547135848cf6d433a0ef47647ade94d06e812ea1c3a3fb7d3bde9046e";
  libraryHaskellDepends = [
    aeson aeson-pretty ansi-terminal async base bytestring co-log-core
    conduit conduit-parse containers data-default Diff directory
    exceptions filepath Glob lens lens-aeson lsp lsp-types mtl
    parser-combinators process row-types some text time transformers
    unix
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
