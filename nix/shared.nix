let
  pinned = import ./pinnedNixpkgs.nix;
  
  defaultCompiler = "ghc843";

in

{ nixpkgs ? pinned.nixpkgs
, nixpkgsStaticLinux ? pinned.nixpkgsStaticLinux
, compiler ? defaultCompiler
, coverage ? false
, system ? builtins.currentSystem
}:

let
  allDhallPackages = [
    "dhall"
    "dhall-bash"
    "dhall-json"
    "dhall-lsp-server"
    "dhall-nix"
    "dhall-yaml"
  ];

  mass = function: names: haskellPackagesNew: haskellPackagesOld:
    let
      toNameValue = name: {
        inherit name;

        value = function haskellPackagesOld."${name}";
      };

    in
      builtins.listToAttrs (map toNameValue names);

  overlayShared = pkgsNew: pkgsOld: {
    sdist = pkgsNew.callPackage ./sdist.nix { };

    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
            overrides =
              let
                applyCoverage = drv:
                  if coverage
                  then
                    pkgsNew.haskell.lib.overrideCabal
                      (pkgsNew.haskell.lib.doCoverage
                        (pkgsNew.haskell.lib.doCheck drv)
                      )
                      (old: {
                          postInstall = (old.postInstall or "") + ''
                            ${pkgsNew.coreutils}/bin/mkdir --parents $out/nix-support
                            ${pkgsNew.coreutils}/bin/ln --symbolic $out/share/hpc/vanilla/html/dhall-* "$out/share/hpc/vanilla/html/dhall"
                            ${pkgsNew.coreutils}/bin/echo "report coverage $out/share/hpc/vanilla/html/dhall/hpc_index.html" >> $out/nix-support/hydra-build-products
                          '';
                        }
                      )
                  else
                    pkgsNew.haskell.lib.dontCheck drv;

                failOnAllWarnings = drv:
                  # GHCJS incorrectly detects non-exhaustive pattern matches
                  if compiler == "ghcjs"
                  then drv
                  else pkgsNew.haskell.lib.failOnAllWarnings drv;

                failOnMissingHaddocks = drv:
                  if compiler == defaultCompiler
                  then
                    drv.overrideAttrs
                    (old: {
                        postHaddock = (old.postHaddock or "") + ''
                          ! (./Setup haddock 2>&1 | grep --quiet 'Missing documentation for:') || (echo "Error: Incomplete haddocks"; exit 1)
                        '';
                      }
                    )
                  else
                    drv;

                doCheckExtension =
                  mass pkgsNew.haskell.lib.doCheck
                    (   [ "dhall-bash"
                          "dhall-json"
                          # The test suite fails due to a relative reference
                          # to ../dhall/dhall-lang/
                          # "dhall-lsp-server"
                          "dhall-nix"
                          "dhall-yaml"
                        ]
                        # Test suite doesn't work on GHCJS
                    ++  pkgsNew.lib.optional (!(compiler == "ghcjs")) "dhall"
                    );

                doBenchmarkExtension =
                  mass pkgsNew.haskell.lib.doBenchmark allDhallPackages;

                failOnAllWarningsExtension =
                  mass failOnAllWarnings [
                    "dhall"
                    "dhall-bash"
                    "dhall-json"
                    "dhall-lsp-server"
                    "dhall-nix"
                    "dhall-yaml"
                  ];

                failOnMissingHaddocksExtension =
                  mass failOnMissingHaddocks [
                    "dhall"
                  ];

                extension =
                  haskellPackagesNew: haskellPackagesOld: {
                    mkDerivation =
                      args: haskellPackagesOld.mkDerivation (args // {
                          doCheck = false;
                        }
                      );

                    dhall =
                      applyCoverage
                        (haskellPackagesNew.callCabal2nix
                          "dhall"
                          (pkgsNew.sdist ../dhall)
                          { }
                        );

                    dhall-no-http =
                      pkgsNew.haskell.lib.appendConfigureFlag
                        haskellPackagesNew.dhall
                        [ "-f-with-http" ];

                    dhall-bash =
                      haskellPackagesNew.callCabal2nix
                        "dhall-bash"
                        (pkgsNew.sdist ../dhall-bash)
                        { };

                    dhall-json =
                      haskellPackagesNew.callCabal2nix
                        "dhall-json"
                        (pkgsNew.sdist ../dhall-json)
                        { };

                    dhall-nix =
                      haskellPackagesNew.callCabal2nix
                        "dhall-nix"
                        (pkgsNew.sdist ../dhall-nix)
                        { };

                    dhall-lsp-server =
                      haskellPackagesNew.callCabal2nix
                        "dhall-lsp-server"
                        (pkgsNew.sdist ../dhall-lsp-server)
                        { };

                    dhall-yaml =
                      haskellPackagesNew.callCabal2nix
                        "dhall-yaml"
                        (pkgsNew.sdist ../dhall-yaml)
                        { };

                    dhall-try =
                      pkgsNew.haskell.lib.overrideCabal
                        (haskellPackagesNew.callCabal2nix
                          "dhall-try"
                          (pkgsNew.sdist ../dhall-try)
                          { }
                        )
                        (old: {
                            postInstall = (old.postInstall or "") + ''
                              ${pkgsNew.closurecompiler}/bin/closure-compiler $out/bin/dhall-try.jsexe/all.js --jscomp_off=checkVars --externs=$out/bin/dhall-try.jsexe/all.js.externs > $out/bin/dhall-try.jsexe/all.min.js
                            '';
                          }
                        );
                  };

              in
                pkgsNew.lib.fold
                  pkgsNew.lib.composeExtensions
                  (old.overrides or (_: _: {}))
                  [ (pkgsNew.haskell.lib.packagesFromDirectory { directory = ./.; })
                    extension
                    doCheckExtension
                    doBenchmarkExtension
                    failOnAllWarningsExtension
                    failOnMissingHaddocksExtension
                  ];
          }
        );
      };
    };
  };

  overlayCabal2nix = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
        overrides =
          let
            extension =
              haskellPackagesNew: haskellPackagesOld: {
                # `cabal2nix` requires a newer version of `hpack`
                hpack =
                  haskellPackagesOld.hpack_0_29_6;
              };

          in
            pkgsNew.lib.composeExtensions
              (old.overrides or (_: _: {}))
              extension;
      }
    );
  };

  overlayGHC822 = pkgsNew: pkgsOld: {
    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
            overrides =
              let
                extension =
                  haskellPackagesNew: haskellPackagesOld: {
                    # Compilation of Dhall.Test.QuickCheck tends to OOM
                    dhall =
                      pkgsNew.haskell.lib.dontCheck haskellPackagesOld.dhall;

                    lens-family-core =
                      haskellPackagesOld.lens-family-core_1_2_1;

                    lens-family = haskellPackagesOld.lens-family_1_2_1;
                  };

              in
                pkgsNew.lib.composeExtensions
                  (old.overrides or (_: _: {}))
                  extension;
          }
        );
      };
    };
  };

  overlayGHC861 = pkgsNew: pkgsOld: {
    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
            overrides =
              let
                extension =
                  haskellPackagesNew: haskellPackagesOld: {
                    lens-family-core =
                        haskellPackagesOld.lens-family-core_1_2_3;

                    # GHC 8.6.1 accidentally shipped with an unpublished
                    # unix-2.8 package.  Normally we'd deal with that by
                    # using `pkgsNew.haskell.lib.jailbreak` but it doesn't
                    # work for dependencies guarded by conditions.  See:
                    # 
                    # https://github.com/peti/jailbreak-cabal/issues/7
                    turtle =
                      pkgsNew.haskell.lib.appendPatch
                        haskellPackagesOld.turtle
                        ./turtle.patch;
                  };

              in
                pkgsNew.lib.composeExtensions
                  (old.overrides or (_: _: {}))
                  extension;
          }
        );
      };
    };
  };

  pkgs = import nixpkgs {
    inherit system;

    config = {};

    overlays =
          [ overlayShared overlayCabal2nix ]
      ++  (      if compiler == "ghc822" then [ overlayGHC822 ]
            else if compiler == "ghc861" then [ overlayGHC861 ]
            else                              [               ]
          );
  };

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
                pkgsNew.haskell.packages."${compiler}".Cabal_patched
              ];

            libraryHaskellDepends =
              (old.libraryHaskellDepends or []) ++ [
                pkgsNew.haskell.packages."${compiler}".Cabal_patched
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

      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
            overrides =
              let
                extension =
                  haskellPackagesNew: haskellPackagesOld: {
                    Cabal_patched =
                      haskellPackagesNew.callCabal2nix
                        "Cabal"
                        pkgsNew.Cabal_patched_Cabal_subdir
                        { };

                    dhall-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall;

                    dhall-bash-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-bash;

                    dhall-json-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-json;

                    dhall-lsp-server-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-lsp-server;

                    dhall-nix-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-nix;

                    dhall-yaml-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-yaml;
                  };

              in
                pkgsNew.lib.composeExtensions
                  (old.overrides or (_: _: {}))
                  extension;
          }
        );
      };
    };
  };

  pkgsStaticLinux = import nixpkgsStaticLinux {
    config = {};
    overlays = [ overlayShared overlayStaticLinux ];
    system = "x86_64-linux";
  };

  trivial = x: pkgs.runCommand "trivial" { inherit x; } "touch $out";

  makeStaticIfPossible = name:
    if pkgs.stdenv.isLinux
    then
      pkgsStaticLinux.pkgsMusl.haskell.packages."${compiler}"."${name}-static"
    else
      pkgs.haskell.lib.justStaticExecutables (pkgs.haskell.packages."${compiler}"."${name}");

  makeTarball = name:
    pkgsStaticLinux.releaseTools.binaryTarball rec {
      src = pkgsStaticLinux.pkgsMusl.haskell.packages."${compiler}"."${name}-static";

      installPhase = ''
        releaseName=${name}
        ${pkgsStaticLinux.coreutils}/bin/install --target-directory "$TMPDIR/inst/bin" -D $src/bin/*
      '';
    };

  toShell = drv: drv.env;

  possibly-static = {
    dhall            = makeStaticIfPossible "dhall"           ;
    dhall-bash       = makeStaticIfPossible "dhall-bash"      ;
    dhall-json       = makeStaticIfPossible "dhall-json"      ;
    dhall-lsp-server = makeStaticIfPossible "dhall-lsp-server";
    dhall-nix        = makeStaticIfPossible "dhall-nix"       ;
    dhall-yaml       = makeStaticIfPossible "dhall-yaml"      ;
  };

  toDockerImage = name:
    let
      image =
        pkgs.dockerTools.buildImage {
          inherit name;

          contents = [ possibly-static."${name}" pkgs.cacert ];
        };

    in
      pkgs.runCommand "image-${name}" {} ''
        ${pkgs.coreutils}/bin/mkdir --parents "$out/nix-support"
        ${pkgs.coreutils}/bin/ln --symbolic '${image}' "$out/docker-image-${name}.tar.gz"
        echo "file binary-dist $out/docker-image-${name}.tar.gz" >> $out/nix-support/hydra-build-products
      '';

in
  rec {
    inherit trivial pkgs possibly-static;

    tarball-dhall            = makeTarball "dhall"           ;
    tarball-dhall-bash       = makeTarball "dhall-bash"      ;
    tarball-dhall-json       = makeTarball "dhall-json"      ;
    tarball-dhall-lsp-server = makeTarball "dhall-lsp-server";
    tarball-dhall-nix        = makeTarball "dhall-nix"       ;
    tarball-dhall-yaml       = makeTarball "dhall-yaml"      ;

    inherit (pkgs) tarball-website website;

    inherit (pkgs.haskell.packages."${compiler}")
      dhall
      dhall-no-http
      dhall-bash
      dhall-json
      dhall-lsp-server
      dhall-nix
      dhall-try
      dhall-yaml
    ;

    inherit (pkgs.releaseTools) aggregate;

    shell-dhall            = pkgs.haskell.packages."${compiler}".dhall.env           ;
    shell-dhall-bash       = pkgs.haskell.packages."${compiler}".dhall-bash.env      ;
    shell-dhall-json       = pkgs.haskell.packages."${compiler}".dhall-json.env      ;
    shell-dhall-lsp-server = pkgs.haskell.packages."${compiler}".dhall-lsp-server.env;
    shell-dhall-nix        = pkgs.haskell.packages."${compiler}".dhall-nix.env       ;
    shell-dhall-try        = pkgs.haskell.packages."${compiler}".dhall-try.env       ;
    shell-dhall-yaml       = pkgs.haskell.packages."${compiler}".dhall-yaml.env      ;

    image-dhall            = toDockerImage "dhall"           ;
    image-dhall-bash       = toDockerImage "dhall-bash"      ;
    image-dhall-json       = toDockerImage "dhall-json"      ;
    image-dhall-lsp-server = toDockerImage "dhall-lsp-server";
    image-dhall-nix        = toDockerImage "dhall-nix"       ;
    image-dhall-yaml       = toDockerImage "dhall-yaml"      ;

    test-dhall =
      pkgs.mkShell
        { buildInputs =
            [ (pkgs.haskell.packages."${compiler}".ghcWithPackages
                (pkgs: [ pkgs.dhall ])
              )
            ];
        };
  }
