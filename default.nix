let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  readDirectory = import ./nix/readDirectory.nix;

  overlayShared = pkgsNew: pkgsOld: {
    dhall-sdist =
      let
        predicate = path: type:
          let
            base = baseNameOf path;

          in
             !( pkgsNew.lib.hasSuffix ".nix" base
             || base == "dist"
             || base == "result"
             || base == ".git"
             );

        src = builtins.filterSource predicate ./.;

      in
        pkgsNew.callPackage (import ./nix/dhall-sdist.nix src) { };

    haskellPackages = pkgsOld.haskellPackages.override (old: {
        overrides =
          let
            extension =
              haskellPackagesNew: haskellPackagesOld: {

                dhall =
                  pkgsNew.haskell.lib.failOnAllWarnings
                    (haskellPackagesNew.callCabal2nix
                      "dhall"
                      pkgsNew.dhall-sdist
                      { }
                    );

                 hpack =
                   haskellPackagesOld.hpack_0_29_6;

                 prettyprinter =
                   pkgs.haskell.lib.dontCheck haskellPackagesOld.prettyprinter;

                 serialise =
                   pkgs.haskell.lib.dontCheck haskellPackagesOld.serialise;
              };

          in
            pkgsNew.lib.fold
              pkgsNew.lib.composeExtensions
              (old.overrides or (_: _: {}))
              [ (readDirectory ./nix)

                extension
              ];
      }
    );
  };

  nixpkgs = fetchNixpkgs {
    rev = "1d4de0d552ae9aa66a5b8dee5fb0650a4372d148";

    sha256 = "09qx58dp1kbj7cpzp8ahbqfbbab1frb12sh1qng87rybcaz0dz01";

    outputSha256 = "0xpqc1fhkvvv5dv1zmas2j1q27mi7j7dgyjcdh82mlgl1q63i660";
  };

  pkgs = import nixpkgs { config = {}; overlays = [ overlayShared ]; };

  overlayStaticLinux = pkgsNew: pkgsOld: {
    cabal_patched_src = pkgsNew.fetchFromGitHub {
      owner = "nh2";
      repo = "cabal";
      rev = "748f07b50724f2618798d200894f387020afc300";
      sha256 = "1k559m291f6spip50rly5z9rbxhfgzxvaz64cx4jqpxgfhbh2gfs";
    };

    Cabal_patched_Cabal_subdir = pkgsNew.stdenv.mkDerivation {
      name = "cabal-dedupe-src";
      buildCommand = ''
        cp -rv ${pkgsNew.cabal_patched_src}/Cabal/ $out
      '';
    };

    haskell = pkgsOld.haskell // {
      lib = pkgsOld.haskell.lib // {
        useFixedCabal = drv: pkgsNew.haskell.lib.overrideCabal drv (old: {
            setupHaskellDepends =
              (old.setupHaskellDepends or []) ++ [
                pkgsNew.haskellPackages.Cabal_patched
              ];

            libraryHaskellDepends =
              (old.libraryHaskellDepends or []) ++ [
                pkgsNew.haskellPackages.Cabal_patched
              ];
          }
        );

      statify = drv:
        pkgsNew.lib.foldl pkgsNew.haskell.lib.appendConfigureFlag
          (pkgsNew.haskell.lib.disableLibraryProfiling
            (pkgsNew.haskell.lib.disableSharedExecutables
              (pkgsNew.haskell.lib.useFixedCabal
                 (pkgsNew.haskell.lib.justStaticExecutables drv)
              )
            )
          )
          [ "--enable-executable-static"
            "--extra-lib-dirs=${pkgsNew.gmp6.override { withStatic = true; }}/lib"
            "--extra-lib-dirs=${pkgsNew.zlib.static}/lib"
            "--extra-lib-dirs=${pkgsNew.ncurses.override { enableStatic = true; }}/lib"
          ];
      };
    };

    haskellPackages = pkgsOld.haskellPackages.override (old: {
        overrides =
          let
            extension =
              haskellPackagesNew: haskellPackagesOld: {
                Cabal_patched =
                  pkgsNew.haskellPackages.callCabal2nix
                    "Cabal"
                    pkgsNew.Cabal_patched_Cabal_subdir
                    { };

                dhall = pkgsNew.haskell.lib.statify haskellPackagesOld.dhall;
              };

          in
            pkgsNew.lib.composeExtensions
              (old.overrides or (_: _: {}))
              extension;
      }
    );
  };

  nixpkgsStaticLinux = fetchNixpkgs {
    owner = "nh2";

    rev = "925aac04f4ca58aceb83beef18cb7dae0715421b";

    sha256 = "0zkvqzzyf5c742zcl1sqc8009dr6fr1fblz53v8gfl63hzqwj0x4";

    outputSha256 = "1zr8lscjl2a5cz61f0ibyx55a94v8yyp6sjzjl2gkqjrjbg99abx";
  };

  pkgsStaticLinux = import nixpkgsStaticLinux {
    config = {};
    overlays = [ overlayShared overlayStaticLinux ];
    system = "x86_64-linux";
  };

  # Derivation that trivially depends on the current directory so that Hydra's
  # pull request builder always posts a GitHub status on each revision
  pwd = pkgs.runCommand "pwd" { here = ./.; } "touch $out";

in
  rec {
    inherit pwd;

    tarball =
      pkgsStaticLinux.releaseTools.binaryTarball rec {
        src = pkgsStaticLinux.pkgsMusl.haskellPackages.dhall;

        installPhase = ''
          releaseName=${src.name}
          ${pkgsStaticLinux.coreutils}/bin/install -D "$src/bin/dhall" "$TMPDIR/inst/bin/dhall"
        '';
      };

    inherit (pkgs.haskellPackages) dhall;

    all = pkgs.releaseTools.aggregate
      { name = "dhall";

        constituents = [
          dhall
          tarball
          pwd
        ];
      };

    shell = (pkgs.haskell.lib.doBenchmark pkgs.haskellPackages.dhall).env;
  }
