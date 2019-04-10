{ mkDerivation, array, base, filepath, ghc-prim, happy
, haskell-lexer, pretty, stdenv, text
}:
mkDerivation {
  pname = "pretty-show";
  version = "1.9.5";
  sha256 = "b095bebb79951d2e25a543a591844fb638165672d7b95d325844611297ba423f";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array base filepath ghc-prim haskell-lexer pretty text
  ];
  libraryToolDepends = [ happy ];
  executableHaskellDepends = [ base ];
  homepage = "http://wiki.github.com/yav/pretty-show";
  description = "Tools for working with derived `Show` instances and generic inspection of values";
  license = stdenv.lib.licenses.mit;
}
