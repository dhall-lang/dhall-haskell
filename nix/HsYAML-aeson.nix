{ mkDerivation, aeson, base, bytestring, containers, fetchgit
, HsYAML, mtl, scientific, stdenv, text, unordered-containers
, vector
}:
mkDerivation {
  pname = "HsYAML-aeson";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/haskell-hvr/HsYAML-aeson.git";
    sha256 = "1snayzarsxhlf29pi1zqryxpjpvp66rjp4ij422lmvn9g3792l4j";
    rev = "9f28206e68f182afd635c71ac628119ea38f63e4";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base bytestring containers HsYAML mtl scientific text
    unordered-containers vector
  ];
  description = "JSON to YAML Adapter";
  license = stdenv.lib.licenses.gpl2Plus;
}
