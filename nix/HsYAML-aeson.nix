{ mkDerivation, aeson, base, bytestring, containers, fetchgit
, HsYAML, mtl, ordered-containers, scientific, stdenv, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "HsYAML-aeson";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/vijayphoenix/HsYAML-aeson.git";
    sha256 = "0a2p4nmfsnvdp1vsaas1pgryx4fv5njdxw4l6vfl0aza3i7ld7kd";
    rev = "e269cc466b43767a6b972a9ce946419641a80e1d";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base bytestring containers HsYAML mtl ordered-containers
    scientific text unordered-containers vector
  ];
  description = "JSON to YAML Adapter";
  license = stdenv.lib.licenses.gpl2Plus;
}
