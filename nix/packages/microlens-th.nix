{ mkDerivation, base, containers, microlens, stdenv
, template-haskell, th-abstraction, transformers
}:
mkDerivation {
  pname = "microlens-th";
  version = "0.4.3.5";
  sha256 = "d03d99d51e9730350ac400edc44e96f90f7e4ad58975dd203b1ac2cc51e4657e";
  libraryHaskellDepends = [
    base containers microlens template-haskell th-abstraction
    transformers
  ];
  testHaskellDepends = [ base microlens ];
  homepage = "http://github.com/monadfix/microlens";
  description = "Automatic generation of record lenses for microlens";
  license = stdenv.lib.licenses.bsd3;
}
