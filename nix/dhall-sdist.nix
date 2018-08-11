src:

{ cabal-install, coreutils, runCommand }:

runCommand "dhall-sdist" {} ''
  ${coreutils}/bin/mkdir $out
  ${coreutils}/bin/cp --recursive ${src} src
  cd src
  ${coreutils}/bin/chmod --recursive u+w .
  ${coreutils}/bin/mkdir dist
  HOME=$PWD ${cabal-install}/bin/cabal sdist --output-directory=$out
''
