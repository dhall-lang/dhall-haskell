{ cabal-install, coreutils, glibcLocales, lib, runCommand, stdenv, tar }:

src:

runCommand "dhall-sdist"
  (   { LANG = "en_US.UTF-8"; }
  //  lib.optionalAttrs (stdenv.buildPlatform.libc == "glibc")
        { LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive"; }
  )
  ''
  ${coreutils}/bin/mkdir $out
  HOME=$PWD ${cabal-install}/bin/cabal v2-sdist --output-dir=- ${src} \
  | ${tar}/bin/tar --extract -z --strip-components=1 -C $out -f -
  ''
