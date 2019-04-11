{ mkDerivation, base, containers, haskell-src-exts, HUnit, pretty
, stdenv, syb, template-haskell, test-framework
, test-framework-hunit, th-orphans
}:
mkDerivation {
  pname = "haskell-src-meta";
  version = "0.8.2";
  sha256 = "7970d29ef9a96dc21c4ac94b4072c7dddec663cf64e8b5ab221d21868cc1166f";
  libraryHaskellDepends = [
    base haskell-src-exts pretty syb template-haskell th-orphans
  ];
  testHaskellDepends = [
    base containers haskell-src-exts HUnit pretty syb template-haskell
    test-framework test-framework-hunit
  ];
  description = "Parse source to template-haskell abstract syntax";
  license = stdenv.lib.licenses.bsd3;
}
