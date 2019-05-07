{ mkDerivation, base, bytestring, conduit, resourcet, stdenv }:
mkDerivation {
  pname = "libyaml";
  version = "0.1.1.0";
  sha256 = "6a034047f45c2f5d3acb8a5b0852075bd4b4f7d6a222a992f45afac152b55f5f";
  libraryHaskellDepends = [ base bytestring conduit resourcet ];
  homepage = "https://github.com/snoyberg/yaml#readme";
  description = "Low-level, streaming YAML interface";
  license = stdenv.lib.licenses.bsd3;
}
