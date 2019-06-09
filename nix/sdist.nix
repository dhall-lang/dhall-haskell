{ cabal-install, coreutils, glibcLocales, lib, runCommand, stdenv }:

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
       || base == "index.html"
       );

  filteredSource = builtins.filterSource predicate src;

in
  runCommand "dhall-sdist"
    (   { LANG = "en_US.UTF-8"; }
    //  lib.optionalAttrs (stdenv.buildPlatform.libc == "glibc")
          { LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive"; }
    )
    ''
    ${coreutils}/bin/mkdir $out
    ${coreutils}/bin/cp --recursive ${filteredSource} src
    cd src
    ${coreutils}/bin/chmod --recursive u+w .
    ${coreutils}/bin/mkdir dist
    HOME=$PWD ${cabal-install}/bin/cabal sdist --output-directory=$out
    ''
