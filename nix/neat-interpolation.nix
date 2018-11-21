{ mkDerivation, base, base-prelude, HTF, megaparsec, stdenv
, template-haskell, text
}:
mkDerivation {
  pname = "neat-interpolation";
  version = "0.3.2.4";
  sha256 = "de7370d938ffd8c7b52d732f4f088387ed8216cf9767d818e99b7ec827931752";
  libraryHaskellDepends = [
    base base-prelude megaparsec template-haskell text
  ];
  testHaskellDepends = [ base-prelude HTF ];
  homepage = "https://github.com/nikita-volkov/neat-interpolation";
  description = "A quasiquoter for neat and simple multiline text interpolation";
  license = stdenv.lib.licenses.mit;
}
