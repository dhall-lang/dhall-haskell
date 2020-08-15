{ mkDerivation, ansi-terminal, async, base, clock, containers, mtl
, optparse-applicative, stdenv, stm, tagged, unbounded-delays, unix
, wcwidth
}:
mkDerivation {
  pname = "tasty";
  version = "1.1.0.3";
  sha256 = "426cf5f542100af2d49585e131819b9e03bceded6f06c5a8c958c2514e8b3193";
  libraryHaskellDepends = [
    ansi-terminal async base clock containers mtl optparse-applicative
    stm tagged unbounded-delays unix wcwidth
  ];
  homepage = "https://github.com/feuerbach/tasty";
  description = "Modern and extensible testing framework";
  license = stdenv.lib.licenses.mit;
}
