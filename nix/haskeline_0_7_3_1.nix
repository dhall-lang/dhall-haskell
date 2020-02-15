{ mkDerivation, base, bytestring, containers, directory, filepath
, process, stdenv, stm, transformers, unix
}:
mkDerivation {
  pname = "haskeline";
  version = "0.7.4.3";
  sha256 = "046d0930bc2dbc57a7cd9ddb5d1e92c7fdb71c6b91b2bbf673f5406843d6b679";
  configureFlags = [ "-f-terminfo" ];
  libraryHaskellDepends = [
    base bytestring containers directory filepath process stm
    transformers unix
  ];
  homepage = "https://github.com/judah/haskeline";
  description = "A command-line interface for user input, written in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
