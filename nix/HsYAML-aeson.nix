{ mkDerivation, aeson, base, bytestring, HsYAML, mtl, stdenv, text
, vector
}:
mkDerivation {
  pname = "HsYAML-aeson";
  version = "0.1.0.0";
  sha256 = "dc06c55a7497a21c6a6bb1070e7c68e6decb536fe8991a95220dbe84147fc1c1";
  revision = "1";
  editedCabalFile = "1kf35mnvc2syly35c2ffl8xxcw4h6lxv9kqirzj2in1ms19df41y";
  libraryHaskellDepends = [
    aeson base bytestring HsYAML mtl text vector
  ];
  description = "JSON to YAML Adapter";
  license = stdenv.lib.licenses.gpl2Plus;
}
