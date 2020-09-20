{ cabal-install, coreutils, glibcLocales, gnutar, lib, runCommand, stdenv }:

src:

let
  predicate = path: type:
    let
      base = baseNameOf path;

    in
       !( lib.hasSuffix ".nix" base
       || base == "dist"
       || base == "result"
       || base == ".git"
       );

  filteredSource = builtins.filterSource predicate src;

in
  runCommand "dhall-sdist"
    (   { LANG = "en_US.UTF-8"; }
    //  lib.optionalAttrs (stdenv.buildPlatform.libc == "glibc")
          { LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive"; }
    )
    ''
    ${coreutils}/bin/cp --recursive ${filteredSource} src
    cd src
    ${coreutils}/bin/chmod --recursive u+w .
    ${coreutils}/bin/mkdir dist tmp
    HOME=$PWD ${cabal-install}/bin/cabal sdist --verbose=0 --output-dir=tmp
    ${gnutar}/bin/tar --directory tmp --extract --file tmp/*.tar.gz
    ${coreutils}/bin/rm tmp/*.tar.gz
    ${coreutils}/bin/mv tmp/* $out
    ''
