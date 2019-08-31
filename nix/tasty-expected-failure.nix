{ mkDerivation, base, stdenv, tagged, tasty }:
mkDerivation {
  pname = "tasty-expected-failure";
  version = "0.11.1.1";
  sha256 = "519a5c0d2ef9dd60355479f11ca47423133364f20ad3151f3c8b105313405ac4";
  revision = "1";
  editedCabalFile = "1b3fn7d3zwhhwm3gp8cmmsdcrvn9dhshd665xrx1mk6cmy4m8k16";
  libraryHaskellDepends = [ base tagged tasty ];
  homepage = "http://github.com/nomeata/tasty-expected-failure";
  description = "Mark tasty tests as failure expected";
  license = stdenv.lib.licenses.mit;
}
