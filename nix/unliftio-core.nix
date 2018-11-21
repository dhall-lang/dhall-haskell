{ mkDerivation, base, stdenv, transformers }:
mkDerivation {
  pname = "unliftio-core";
  version = "0.1.2.0";
  sha256 = "24c38b3d610ca2642ed496d1de3d7b6b398ce0410aa0a15f3c7ce636ba8f7a78";
  revision = "1";
  editedCabalFile = "0s6xfg9d0i3sfil5gjbamlq017wdxa69csk73bcqjkficg43vm29";
  libraryHaskellDepends = [ base transformers ];
  homepage = "https://github.com/fpco/unliftio/tree/master/unliftio-core#readme";
  description = "The MonadUnliftIO typeclass for unlifting monads to IO";
  license = stdenv.lib.licenses.mit;
}
