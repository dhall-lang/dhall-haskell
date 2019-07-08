    let OperatingSystem = < Linux : {} | OSX : {} >

in  let Addon = { apt : { packages : List Text, sources : List Text } }

in  let makeInclude =
            λ ( args
              : { ghc :
                    Text
                , cabal :
                    Text
                , deploy :
                    Bool
                , os :
                    OperatingSystem
                }
              )
          →     let release =
                      if args.deploy then " DEPLOY_GITHUB_RELEASE=TRUE" else ""
            
            in  { env =
                    "CABALVER=${args.cabal} GHCVER=${args.ghc}${release}"
                , compiler =
                    ": #GHC ${args.ghc}"
                , addons =
                    merge
                    { Linux =
                          λ(_ : {})
                        → [ { apt =
                                { packages =
                                    [ "cabal-install-${args.cabal}"
                                    , "ghc-${args.ghc}"
                                    ]
                                , sources =
                                    [ "hvr-ghc" ]
                                }
                            }
                          ] : Optional Addon
                    , OSX =
                        λ(_ : {}) → [] : Optional Addon
                    }
                    args.os
                , os =
                    merge
                    { Linux =
                        λ(_ : {}) → [] : Optional Text
                    , OSX =
                        λ(_ : {}) → [ "osx" ] : Optional Text
                    }
                    args.os
                }

in  { language =
        "c"
    , sudo =
        False
    , cache =
        { directories = [ "\$HOME/.cabsnap", "\$HOME/.cabal/packages" ] }
    , before_cache =
        [ "rm -fv \$HOME/.cabal/packages/hackage.haskell.org/build-reports.log"
        , "rm -fv \$HOME/.cabal/packages/hackage.haskell.org/00-index.tar"
        ]
    , matrix =
        { include =
            [ makeInclude
              { ghc =
                  "7.10.2"
              , cabal =
                  "1.22"
              , deploy =
                  False
              , os =
                  OperatingSystem.Linux {=}
              }
            , makeInclude
              { ghc =
                  "8.0.1"
              , cabal =
                  "1.24"
              , deploy =
                  True
              , os =
                  OperatingSystem.Linux {=}
              }
            , makeInclude
              { ghc =
                  "8.0.1"
              , cabal =
                  "1.24"
              , deploy =
                  True
              , os =
                  OperatingSystem.OSX {=}
              }
            ]
        }
    , before_install =
        [ "unset CC"
        , "export PATH=/opt/ghc/\$GHCVER/bin:/opt/cabal/\$CABALVER/bin:\$PATH"
        , ''
          if [ "$TRAVIS_OS_NAME" = osx ];
          then
            brew update;
            brew install cabal-install;
            brew install gnu-sed --with-default-names;
          fi
          ''
        ]
    , install =
        [ "cabal --version"
        , "echo \"\$(ghc --version) [\$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]\""
        , ''
          if [ -f $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz ];
          then
            zcat $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz >
                 $HOME/.cabal/packages/hackage.haskell.org/00-index.tar;
          fi
          ''
        , "travis_retry cabal update -v"
        , "sed -i 's/^jobs:/-- jobs:/' \${HOME}/.cabal/config"
        , "cabal install --only-dependencies --enable-tests --enable-benchmarks --dry -v > installplan.txt"
        , "sed -i -e '1,/^Resolving /d' installplan.txt; cat installplan.txt"
        , ''
          if diff -u $HOME/.cabsnap/installplan.txt installplan.txt;
          then
            echo "cabal build-cache HIT";
            rm -rfv .ghc;
            cp -a $HOME/.cabsnap/ghc $HOME/.ghc;
            cp -a $HOME/.cabsnap/lib $HOME/.cabsnap/share $HOME/.cabsnap/bin $HOME/.cabal/;
          else
            echo "cabal build-cache MISS";
            rm -rf $HOME/.cabsnap;
            mkdir -p $HOME/.ghc $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin;
            cabal install --only-dependencies --enable-tests --enable-benchmarks;
          fi
          ''
        , ''
          if [ ! -d $HOME/.cabsnap ];
          then
             echo "snapshotting package-db to build-cache";
             mkdir $HOME/.cabsnap;
             cp -a $HOME/.ghc $HOME/.cabsnap/ghc;
             cp -a $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin installplan.txt $HOME/.cabsnap/;
          fi
          ''
        ]
    , script =
        [ "if [ -f configure.ac ]; then autoreconf -i; fi"
        , "cabal configure --enable-tests --enable-benchmarks -v2"
        , "cabal build"
        , "cabal test"
        , "cabal check"
        , "cabal sdist"
        , ''
          SRC_TGZ=$(cabal info . | awk '{print $2;exit}').tar.gz &&
          (cd dist && cabal install --force-reinstalls "$SRC_TGZ")
          ''
        ]
    , before_deploy =
        [ "tar --create --file \"\$TRAVIS_OS_NAME.tar\" --files-from /dev/null"
        , "tar --append --file \"\$TRAVIS_OS_NAME.tar\" --directory dist/build/dhall-to-json dhall-to-json"
        , "tar --append --file \"\$TRAVIS_OS_NAME.tar\" --directory dist/build/dhall-to-yaml dhall-to-yaml"
        , "gzip \"\$TRAVIS_OS_NAME.tar\""
        ]
    , deploy =
        { provider =
            "releases"
        , api_key =
            "\$GITHUB_OAUTH_TOKEN"
        , file =
            "\$TRAVIS_OS_NAME.tar.gz"
        , on =
            { condition = "\$DEPLOY_GITHUB_RELEASE = true", tags = True }
        , skip_cleanup =
            True
        }
    }
