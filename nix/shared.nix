{ compiler ? "ghc843", coverage ? false, system ? builtins.currentSystem }:

let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  mass = function: names: haskellPackagesNew: haskellPackagesOld:
    let
      toNameValue = name: {
        inherit name;

        value = function haskellPackagesOld."${name}";
      };

    in
      builtins.listToAttrs (map toNameValue names);

  overlayShared = pkgsNew: pkgsOld: {
    logo = {
      bash =
        pkgsNew.fetchurl {
          url    = "https://raw.githubusercontent.com/odb/official-bash-logo/master/assets/Logos/Icons/PNG/128x128.png";
          sha256 = "0fybbp6hbqrfw80fbk55bnykzda0m7x4vk38i80bjlmfbrkfvild";
        };

      clojure =
        pkgsNew.fetchurl {
          url    = "https://upload.wikimedia.org/wikipedia/commons/5/5d/Clojure_logo.svg";
          sha256 = "0mrjzv690g9mxljzxsvay8asyr8vlxhhs9smmax7mp3psd49b43g";
        };

      ruby =
        pkgsNew.fetchurl {
          url    = "https://upload.wikimedia.org/wikipedia/commons/7/73/Ruby_logo.svg";
          sha256 = "1yvvdqcmgpa75y7px3isi4x6690iksq52ilnbslhn7mcngikw6m9";
        };

      dhallLarge =
        pkgsNew.fetchurl {
          url    = "https://raw.githubusercontent.com/dhall-lang/dhall-lang/8bab26f9515cc1007025e0ab4b4e7dd6e95a7103/img/dhall-logo.png";
          sha256 = "0j6sfvm4kxqb2m6s1sv9qag7m30cibaxpphprhaibp9s9shpra4p";
        };

      dhallSmall =
        pkgsNew.fetchurl {
          url    = "https://raw.githubusercontent.com/dhall-lang/dhall-lang/8bab26f9515cc1007025e0ab4b4e7dd6e95a7103/img/dhall-icon.png";
          sha256 = "1lly3yb5szl9n3hszsfzv2mil98cvlidrzyci7vs4wi461s9bhxi";
        };

      discourse = ./img/discourse.svg;

      github = pkgsNew.callPackage ./githubLogo.nix { };

      haskell =
        pkgsNew.fetchurl {
          url    = "https://wiki.haskell.org/wikiupload/4/4a/HaskellLogoStyPreview-1.png";
          sha256 = "0g26j7vx34m46mwp93qgg3q5x8pfdq2j1ch0vxz5gj0nk3b8fxda";
        };

      kubernetes =
        pkgsNew.fetchurl {
          url    = "https://raw.githubusercontent.com/kubernetes/kubernetes/7839fe38620508eb0651930cb0e1acb8ea367842/logo/logo.svg";
          sha256 = "0kp6idffg9k52ycgv5zkg9n08pfldzsy0fzhwsrb2f7cvrl6fpw4";
        };

      nix =
        pkgsNew.fetchurl {
          url    = "https://nixos.org/logo/nix-wiki.png";
          sha256 = "1hrz7wr7i0b2bips60ygacbkmdzv466lsbxi22hycg42kv4m0173";
        };

      json =
        pkgsNew.fetchurl {
          url    = "https://upload.wikimedia.org/wikipedia/commons/c/c9/JSON_vector_logo.svg";
          sha256 = "1hqd1qh35v9magjp3rbsw8wszk2wn3hkz981ir49z5cyf11jnx95";
        };

      stackOverflow =
        pkgsNew.fetchurl {
          url    = "https://cdn.sstatic.net/Sites/stackoverflow/company/img/logos/so/so-icon.svg";
          sha256 = "0i84h23ax197f3hwh0hqm6yjvvnpcjyhd6nkyy33z6x10dh8v4z3";
        };


      twitter = pkgsNew.callPackage ./twitterLogo.nix { };

      yaml =
        pkgsNew.fetchurl {
          url    = "https://raw.githubusercontent.com/yaml/yaml-spec/a6f764e13de58d5f753877f588a01b35dc9a5168/logo.png";
          sha256 = "12grgaxpqi755p2rnvw3x02zc69brpnzx208id1f0z42w387j4hi";
        };
    };

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
                  # Older versions of GHC incorrectly detect non-exhaustive
                  # pattern matches
                  if compiler == "ghc7103" || compiler == "ghcjs"
                  then drv
                  else pkgsNew.haskell.lib.failOnAllWarnings drv;

                doCheckExtension =
                  mass pkgsNew.haskell.lib.doCheck
                    (   [ "dhall-bash"
                          "dhall-json"
                          # The test suite fails due to a relative reference
                          # to ../dhall/dhall-lang/
                          # "dhall-lsp-server"
                          "dhall-nix"
                          "dhall-text"
                        ]
                        # Test suite doesn't work on GHCJS or GHC 7.10.3
                    ++  pkgsNew.lib.optional (!(compiler == "ghcjs" || compiler == "ghc7103")) "dhall"
                    );

                failOnAllWarningsExtension =
                  mass failOnAllWarnings [
                    "dhall"
                    "dhall-bash"
                    "dhall-json"
                    "dhall-text"
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

                    dhall-lsp-server =
                      haskellPackagesNew.callCabal2nix
                        "dhall-lsp-server"
                        (pkgsNew.sdist ../dhall-lsp-server)
                        { };

                    dhall-nix =
                      haskellPackagesNew.callCabal2nix
                        "dhall-nix"
                        (pkgsNew.sdist ../dhall-nix)
                        { };

                    dhall-text =
                      haskellPackagesNew.callCabal2nix
                        "dhall-text"
                        (pkgsNew.sdist ../dhall-text)
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
                    failOnAllWarningsExtension
                  ];
          }
        );
      };
    };

    npm = pkgsNew.callPackage ./npm { };

    jQuery =
      pkgsNew.fetchurl {
        url    = "https://code.jquery.com/jquery-3.3.1.min.js";
        sha256 = "1vq2bp290rhby5l09dv5khqwv3ysnzbddggbgk6m4hl9y9pl42hn";
      };

    twitterBootstrap = pkgsNew.callPackage ./twitterBootstrap.nix { };

    website = pkgsNew.callPackage ./website.nix {};

    tarball-website = pkgsStaticLinux.releaseTools.binaryTarball rec {
      src = pkgsNew.website;

      installPhase = ''
        releaseName=website
        ${pkgsNew.coreutils}/bin/install --target-directory "$TMPDIR/inst/website/"    -D $src/index.html
        ${pkgsNew.coreutils}/bin/install --target-directory "$TMPDIR/inst/website/img" -D $src/img/*
        ${pkgsNew.coreutils}/bin/install --target-directory "$TMPDIR/inst/website/css" -D $src/css/*
        ${pkgsNew.coreutils}/bin/install --target-directory "$TMPDIR/inst/website/js"  -D $src/js/*
      '';
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

  overlayGHC7103 = pkgsNew: pkgsOld: {
    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
            overrides =
              let
                extension =
                  haskellPackagesNew: haskellPackagesOld: {
                    # Newer version of these packages have bounds incompatible
                    # with GHC 7.10.3
                    lens-family-core =
                      haskellPackagesOld.lens-family-core_1_2_1;

                    memory =
                      haskellPackagesOld.memory_0_14_16;

                    basement =
                      haskellPackagesOld.basement_0_0_6;

                    foundation =
                      haskellPackagesOld.foundation_0_0_19;

                    # Most of these fixes are due to certain dependencies being
                    # hidden behind a conditional compiler version directive, so
                    # they aren't included by default in the default Hackage
                    # package set (which was generated for `ghc-8.4.3`)
                    base-compat-batteries =
                      pkgsNew.haskell.lib.addBuildDepends
                        haskellPackagesOld.base-compat-batteries
                        [ haskellPackagesNew.bifunctors
                          haskellPackagesNew.fail
                        ];

                    blaze-builder =
                      pkgsNew.haskell.lib.addBuildDepend
                        haskellPackagesOld.blaze-builder
                        haskellPackagesNew.semigroups;

                    cborg =
                      pkgsNew.haskell.lib.addBuildDepends
                        haskellPackagesOld.cborg
                        [ haskellPackagesNew.fail
                          haskellPackagesNew.semigroups
                        ];

                    conduit =
                      pkgsNew.haskell.lib.addBuildDepend
                        haskellPackagesOld.conduit
                        haskellPackagesNew.semigroups;

                    contravariant =
                      pkgsNew.haskell.lib.addBuildDepends
                        haskellPackagesOld.contravariant
                        [ haskellPackagesNew.fail
                          haskellPackagesNew.semigroups
                        ];

                    dhall =
                      pkgsNew.haskell.lib.addBuildDepends
                        haskellPackagesOld.dhall
                        [ haskellPackagesNew.doctest
                          haskellPackagesNew.mockery
                        ];

                    generic-deriving =
                      pkgsNew.haskell.lib.dontCheck
                        haskellPackagesOld.generic-deriving;

                    haskell-src =
                      pkgsNew.haskell.lib.addBuildDepends
                        haskellPackagesOld.haskell-src
                        [ haskellPackagesNew.fail
                          haskellPackagesNew.semigroups
                        ];

                    megaparsec =
                      pkgsNew.haskell.lib.addBuildDepend
                        haskellPackagesOld.megaparsec
                        haskellPackagesNew.fail;

                    neat-interpolation =
                      pkgsNew.haskell.lib.doJailbreak
                        haskellPackagesOld.neat-interpolation;

                    optparse-applicative =
                      pkgsNew.haskell.lib.addBuildDepend
                        haskellPackagesOld.optparse-applicative
                        haskellPackagesNew.fail;

                    parser-combinators =
                      pkgsNew.haskell.lib.addBuildDepend
                        haskellPackagesOld.parser-combinators
                        haskellPackagesNew.semigroups;

                    prettyprinter =
                      pkgsNew.haskell.lib.addBuildDepend
                        haskellPackagesOld.prettyprinter
                        haskellPackagesNew.semigroups;

                    transformers-compat =
                      pkgsNew.haskell.lib.addBuildDepends
                        haskellPackagesOld.transformers-compat
                        [ haskellPackagesNew.fail
                          haskellPackagesNew.generic-deriving
                        ];

                    vector =
                      pkgsNew.haskell.lib.addBuildDepend
                        haskellPackagesOld.vector
                        haskellPackagesNew.semigroups;

                    # For some reason, `Cabal-1.22.5` does not respect the
                    # `buildable: False` directive for the executable section
                    # even when configured with `-f -cli`.  Fixing this requires
                    # patching out the executable section of `wcwidth` in order
                    # to avoid pulling in some extra dependencies which cause a
                    # a dependency cycle.
                    wcwidth =
                      pkgsNew.haskell.lib.appendPatch
                        haskellPackagesOld.wcwidth ./wcwidth.patch;

                    yaml =
                      pkgsNew.haskell.lib.doJailbreak haskellPackagesOld.yaml;
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



  nixpkgs = fetchNixpkgs {
    rev = "1d4de0d552ae9aa66a5b8dee5fb0650a4372d148";

    sha256 = "09qx58dp1kbj7cpzp8ahbqfbbab1frb12sh1qng87rybcaz0dz01";

    outputSha256 = "0xpqc1fhkvvv5dv1zmas2j1q27mi7j7dgyjcdh82mlgl1q63i660";
  };

  pkgs = import nixpkgs {
    inherit system;

    config = {};

    overlays =
          [ overlayShared overlayCabal2nix ]
      ++  (      if compiler == "ghc7103" then [ overlayGHC7103 ]
            else if compiler == "ghc861"  then [ overlayGHC861  ]
            else                               [                ]
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

                    dhall-text-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-text;
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

  toShell = drv:
    # Benchmark dependencies aren't added by default
    (pkgs.haskell.lib.doBenchmark drv).env;

  possibly-static = {
    dhall            = makeStaticIfPossible "dhall"           ;
    dhall-bash       = makeStaticIfPossible "dhall-bash"      ;
    dhall-json       = makeStaticIfPossible "dhall-json"      ;
    dhall-lsp-server = makeStaticIfPossible "dhall-lsp-server";
    dhall-nix        = makeStaticIfPossible "dhall-nix"       ;
    dhall-text       = makeStaticIfPossible "dhall-text"      ;
  };

  toDockerImage = name:
    let
      image =
        pkgs.dockerTools.buildImage {
          inherit name;

          contents = [ possibly-static."${name}" ];
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
    tarball-dhall-text       = makeTarball "dhall-text"      ;

    inherit (pkgs) tarball-website website;

    inherit (pkgs.haskell.packages."${compiler}") dhall dhall-bash dhall-json dhall-lsp-server dhall-nix dhall-text dhall-try;

    inherit (pkgs.releaseTools) aggregate;

    shell-dhall            = toShell pkgs.haskell.packages."${compiler}".dhall           ;
    shell-dhall-bash       = toShell pkgs.haskell.packages."${compiler}".dhall-bash      ;
    shell-dhall-json       = toShell pkgs.haskell.packages."${compiler}".dhall-json      ;
    shell-dhall-lsp-server = toShell pkgs.haskell.packages."${compiler}".dhall-lsp-server;
    shell-dhall-nix        = toShell pkgs.haskell.packages."${compiler}".dhall-nix       ;
    shell-dhall-text       = toShell pkgs.haskell.packages."${compiler}".dhall-text      ;
    shell-dhall-try        = toShell pkgs.haskell.packages."${compiler}".dhall-try       ;

    image-dhall            = toDockerImage "dhall"           ;
    image-dhall-bash       = toDockerImage "dhall-bash"      ;
    image-dhall-json       = toDockerImage "dhall-json"      ;
    image-dhall-lsp-server = toDockerImage "dhall-lsp-server";
    image-dhall-nix        = toDockerImage "dhall-nix"       ;
    image-dhall-text       = toDockerImage "dhall-text"      ;

    test-dhall =
      pkgs.mkShell
        { buildInputs =
            [ (pkgs.haskell.packages."${compiler}".ghcWithPackages
                (pkgs: [ pkgs.dhall ])
              )
            ];
        };
  }
