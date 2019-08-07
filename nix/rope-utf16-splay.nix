{ mkDerivation, base, QuickCheck, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "rope-utf16-splay";
  version = "0.3.1.0";
  sha256 = "cbf878098355441ed7be445466fcb72d45390073a298b37649d762de2a7f8cc6";
  libraryHaskellDepends = [ base text ];
  testHaskellDepends = [
    base QuickCheck tasty tasty-hunit tasty-quickcheck text
  ];
  homepage = "https://github.com/ollef/rope-utf16-splay";
  description = "Ropes optimised for updating using UTF-16 code units and row/column pairs";
  license = stdenv.lib.licenses.bsd3;
}
