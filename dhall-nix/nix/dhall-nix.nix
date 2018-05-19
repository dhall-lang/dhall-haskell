{ mkDerivation, base, containers, data-fix, dhall, formatting, hnix
, insert-ordered-containers, neat-interpolation, optparse-generic
, scientific, stdenv, text
}:
mkDerivation {
  pname = "dhall-nix";
  version = "1.1.4";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers data-fix dhall formatting hnix
    insert-ordered-containers neat-interpolation scientific text
  ];
  executableHaskellDepends = [
    base dhall hnix optparse-generic text
  ];
  description = "Dhall to Nix compiler";
  license = stdenv.lib.licenses.bsd3;
}
