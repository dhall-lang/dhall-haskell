src:

{ cabal-install, coreutils, glibcLocales, lib, runCommand, stdenv }:

runCommand "dhall-sdist"
  (   { LANG = "en_US.UTF-8"; }
  //  lib.optionalAttrs (stdenv.buildPlatform.libc == "glibc")
        { LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive"; }
  )
  ''
  ${coreutils}/bin/mkdir $out
  ${coreutils}/bin/cp --recursive ${src} src
  cd src
  ${coreutils}/bin/chmod --recursive u+w .
  ${coreutils}/bin/mkdir dist
  HOME=$PWD ${cabal-install}/bin/cabal sdist --output-directory=$out
  ''
