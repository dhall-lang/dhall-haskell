{- the fully expanded Dhall normal form of this file is 28 MB -}
let concatMapSep =
      ./Prelude/Text/concatMapSep

let concat =
      ./Prelude/Text/concat

let types = ./cpkg-types.dhall

let prelude = ./cpkg-prelude.dhall

let gpgPackage =
      λ(x : { name : Text, version : List Natural }) →
          prelude.simplePackage x
        ⫽ { pkgUrl =
              "https://gnupg.org/ftp/gcrypt/${x.name}/${x.name}-${prelude.showVersion
                                                                    x.version}.tar.bz2"
          }

let gnupg =
      λ(v : List Natural) →
          gpgPackage { name = "gnupg", version = v }
        ⫽ { pkgDeps =
            [ prelude.lowerBound { name = "npth", lower = [ 1, 2 ] }
            , prelude.lowerBound { name = "libgpg-error", lower = [ 1, 24 ] }
            , prelude.lowerBound { name = "libgcrypt", lower = [ 1, 7, 0 ] }
            , prelude.lowerBound { name = "libassuan", lower = [ 2, 5, 0 ] }
            , prelude.lowerBound { name = "libksba", lower = [ 1, 3, 4 ] }
            ]
          , installCommand = prelude.installWithBinaries [ "bin/gpg" ]
          }

let npth = λ(v : List Natural) → gpgPackage { name = "npth", version = v }

let libgpgError =
      λ(v : List Natural) → gpgPackage { name = "libgpg-error", version = v }

let libgcrypt =
      λ(v : List Natural) →
          gpgPackage { name = "libgcrypt", version = v }
        ⫽ { pkgDeps =
            [ prelude.lowerBound { name = "libgpg-error", lower = [ 1, 25 ] } ]
          }

let libassuan =
      λ(v : List Natural) →
          gpgPackage { name = "libassuan", version = v }
        ⫽ { pkgDeps =
            [ prelude.lowerBound { name = "libgpg-error", lower = [ 1, 24 ] } ]
          }

let libksba =
      λ(v : List Natural) →
          gpgPackage { name = "libksba", version = v }
        ⫽ { pkgDeps =
            [ prelude.lowerBound { name = "libgpg-error", lower = [ 1, 8 ] } ]
          }

let musl =
      λ(v : List Natural) →
          prelude.simplePackage { name = "musl", version = v }
        ⫽ { pkgUrl =
              "https://www.musl-libc.org/releases/musl-${prelude.showVersion
                                                           v}.tar.gz"
          , installCommand = prelude.installWithBinaries [ "bin/musl-gcc" ]
          }

let binutils =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "binutils", version = v }
        ⫽ { pkgUrl =
              "https://ftp.wayne.edu/gnu/binutils/binutils-${prelude.showVersion
                                                               v}.tar.lz"
          , installCommand =
              prelude.installWithBinaries
                [ "bin/ar"
                , "bin/as"
                , "bin/ld"
                , "bin/strip"
                , "bin/strings"
                , "bin/readelf"
                , "bin/objdump"
                , "bin/nm"
                , "bin/ranlib"
                ]
          }

let bison =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "bison", version = v }
        ⫽ { buildCommand =
              λ(cfg : types.BuildVars) →
                prelude.generalBuild
                  prelude.singleThreaded
                  (prelude.buildEnv cfg)
                  cfg
          , installCommand =
              prelude.installWithBinaries [ "bin/bison", "bin/yacc" ]
          , pkgBuildDeps = [ prelude.unbounded "m4" ]
          }

let cmake =
      λ(cfg : { version : List Natural, patch : Natural }) →
        let patchString = Natural/show cfg.patch

        let versionString = prelude.showVersion cfg.version

        let cmakeConfigure =
              λ(cfg : types.BuildVars) →
                prelude.configureMkExesExtraFlags
                  { bins = [ "bootstrap" ]
                  , extraFlags = [ "--parallel=${Natural/show cfg.cpus}" ]
                  }
                  cfg

        in    prelude.defaultPackage
            ⫽ { pkgName = "cmake"
              , pkgVersion = prelude.fullVersion cfg
              , pkgUrl =
                  "https://cmake.org/files/v${versionString}/cmake-${versionString}.${patchString}.tar.gz"
              , pkgSubdir = "cmake-${versionString}.${patchString}"
              , configureCommand = cmakeConfigure
              , installCommand =
                  λ(cfg : types.BuildVars) →
                    let wrapper =
                          "CMAKE_ROOT=${cfg.installDir}/share/cmake-${versionString}/ ${cfg.installDir}/bin/cmake \$@"

                    let wrapped = "wrapper/cmake"

                    in    prelude.defaultInstall cfg
                        # [ prelude.createDir "wrapper"
                          , prelude.writeFile
                              { file = wrapped, contents = wrapper }
                          , prelude.mkExe wrapped
                          , prelude.copyFile wrapped wrapped
                          , prelude.symlinkBinary wrapped
                          ]
              }

let curl =
      λ(v : List Natural) →
          prelude.simplePackage { name = "curl", version = v }
        ⫽ { pkgUrl =
              "https://curl.haxx.se/download/curl-${prelude.showVersion
                                                      v}.tar.xz"
          , installCommand = prelude.installWithBinaries [ "bin/curl" ]
          , pkgDeps = [ prelude.unbounded "zlib", prelude.unbounded "brotli" ]
          }

let dbus =
      λ(v : List Natural) →
          prelude.simplePackage { name = "dbus", version = v }
        ⫽ { pkgUrl =
              "https://dbus.freedesktop.org/releases/dbus/dbus-${prelude.showVersion
                                                                   v}.tar.xz"
          , pkgDeps =
            [ prelude.unbounded "expat", prelude.unbounded "libselinux" ]
          , configureCommand = prelude.configureLinkExtraLibs [ "pcre" ]
          , pkgBuildDeps = [ prelude.unbounded "pkg-config" ]
          }

let fltk =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "fltk", version = v }
            ⫽ { pkgUrl =
                  "http://fltk.org/pub/fltk/${versionString}/fltk-${versionString}-source.tar.bz2"
              , pkgSubdir = "fltk-${versionString}"
              , pkgDeps =
                [ prelude.unbounded "libX11"
                , prelude.unbounded "alsa-lib"
                , prelude.unbounded "zlib"
                , prelude.unbounded "libpng"
                , prelude.unbounded "libXft"
                , prelude.unbounded "freetype"
                ]
              }

let gawk =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "gawk", version = v }
        ⫽ { installCommand =
              prelude.installWithBinaries [ "bin/gawk", "bin/awk" ]
          }

let gc =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "gc", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/ivmai/bdwgc/releases/download/v${versionString}/gc-${versionString}.tar.gz"
              , pkgDeps = [ prelude.unbounded "libatomic_ops" ]
              }

let libatomic_ops =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "libatomic_ops", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/ivmai/libatomic_ops/releases/download/v${versionString}/libatomic_ops-${versionString}.tar.gz"
              }

let git =
      λ(v : List Natural) →
          prelude.simplePackage { name = "git", version = v }
        ⫽ { pkgUrl =
              "https://mirrors.edge.kernel.org/pub/software/scm/git/git-${prelude.showVersion
                                                                            v}.tar.xz"
          , installCommand = prelude.installWithBinaries [ "bin/git" ]
          , pkgBuildDeps = [ prelude.unbounded "gettext" ]
          }

let glibc =
      let buildDir = Some "build"

      let glibcConfigure =
            λ(cfg : types.BuildVars) →
              let maybeHost = prelude.mkHost cfg.targetTriple

              let modifyArgs = prelude.maybeAppend Text maybeHost

              in  [ prelude.createDir "build"
                  , prelude.call
                      { program = "../configure"
                      , arguments = modifyArgs [ "--prefix=${cfg.installDir}" ]
                      , environment = prelude.configSome ([] : List Text) cfg
                      , procDir = buildDir
                      }
                  ]

      let glibcBuild =
            λ(cfg : types.BuildVars) →
              [ prelude.call
                  { program = prelude.makeExe cfg.buildOS
                  , arguments = [ "-j${Natural/show cfg.cpus}" ]
                  , environment = prelude.configSome ([] : List Text) cfg
                  , procDir = buildDir
                  }
              ]

      let glibcInstall =
            λ(cfg : types.BuildVars) →
              [ prelude.call
                  { program = prelude.makeExe cfg.buildOS
                  , arguments = [ "install" ]
                  , environment = prelude.configSome ([] : List Text) cfg
                  , procDir = buildDir
                  }
              ]

      in  λ(v : List Natural) →
              prelude.defaultPackage
            ⫽ { pkgName = "glibc"
              , pkgVersion = v
              , pkgUrl =
                  "http://mirror.keystealth.org/gnu/libc/glibc-${prelude.showVersion
                                                                   v}.tar.xz"
              , pkgSubdir = "glibc-${prelude.showVersion v}"
              , configureCommand = glibcConfigure
              , buildCommand = glibcBuild
              , installCommand = glibcInstall
              , pkgBuildDeps =
                [ prelude.unbounded "bison"
                , prelude.unbounded "gawk"
                , prelude.unbounded "python3"
                ]
              }

let gmp =
      λ(v : List Natural) →
          prelude.simplePackage { name = "gmp", version = v }
        ⫽ { pkgUrl =
              "https://gmplib.org/download/gmp/gmp-${prelude.showVersion
                                                       v}.tar.lz"
          , pkgBuildDeps = [ prelude.unbounded "m4" ]
          }

let harfbuzz =
      let symlinkHarfbuzz =
            λ(h : Text) → prelude.symlink "include/harfbuzz/${h}" "include/${h}"

      in  λ(v : List Natural) →
              prelude.simplePackage { name = "harfbuzz", version = v }
            ⫽ { pkgUrl =
                  let versionString = prelude.showVersion v

                  in  "https://github.com/harfbuzz/harfbuzz/releases/download/${versionString}/harfbuzz-${versionString}.tar.xz"
              , pkgDeps =
                [ prelude.unbounded "freetype-prebuild"
                , prelude.unbounded "glib"
                ]
              , pkgBuildDeps = [ prelude.unbounded "pkg-config" ]
              , configureCommand =
                  prelude.configureLinkExtraLibs [ "pcre", "z" ]
              , installCommand =
                  λ(cfg : types.BuildVars) →
                      prelude.defaultInstall cfg
                    # [ symlinkHarfbuzz "hb-aat-layout.h"
                      , symlinkHarfbuzz "hb-aat.h"
                      , symlinkHarfbuzz "hb-blob.h"
                      , symlinkHarfbuzz "hb-buffer.h"
                      , symlinkHarfbuzz "hb-common.h"
                      , symlinkHarfbuzz "hb-deprecated.h"
                      , symlinkHarfbuzz "hb-face.h"
                      , symlinkHarfbuzz "hb-font.h"
                      , symlinkHarfbuzz "hb-ft.h"
                      , symlinkHarfbuzz "hb-glib.h"
                      , symlinkHarfbuzz "hb-icu.h"
                      , symlinkHarfbuzz "hb-map.h"
                      , symlinkHarfbuzz "hb-ot-color.h"
                      , symlinkHarfbuzz "hb-ot-font.h"
                      , symlinkHarfbuzz "hb-ot-layout.h"
                      , symlinkHarfbuzz "hb-ot-math.h"
                      , symlinkHarfbuzz "hb-ot-name.h"
                      , symlinkHarfbuzz "hb-ot-shape.h"
                      , symlinkHarfbuzz "hb-ot-var.h"
                      , symlinkHarfbuzz "hb-ot.h"
                      , symlinkHarfbuzz "hb-set.h"
                      , symlinkHarfbuzz "hb-shape-plan.h"
                      , symlinkHarfbuzz "hb-shape.h"
                      , symlinkHarfbuzz "hb-subset.h"
                      , symlinkHarfbuzz "hb-unicode.h"
                      , symlinkHarfbuzz "hb-version.h"
                      , symlinkHarfbuzz "hb.h"
                      ]
              }

let libjpeg-turbo =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.cmakePackage
            ⫽ { pkgName = "libjpeg-turbo"
              , pkgVersion = v
              , pkgUrl =
                  "https://downloads.sourceforge.net/libjpeg-turbo/${versionString}/libjpeg-turbo-${versionString}.tar.gz"
              , pkgSubdir = "libjpeg-turbo-${versionString}"
              , pkgBuildDeps =
                [ prelude.unbounded "cmake"
                , prelude.unbounded "nasm"
                , prelude.unbounded "make"
                ]
              }

let libuv =
      λ(v : List Natural) →
          prelude.defaultPackage
        ⫽ { pkgName = "libuv"
          , pkgVersion = v
          , pkgUrl =
              "https://dist.libuv.org/dist/v${prelude.showVersion
                                                v}/libuv-v${prelude.showVersion
                                                              v}.tar.gz"
          , pkgSubdir = "libuv-v${prelude.showVersion v}"
          , configureCommand = prelude.autogenConfigure
          , pkgBuildDeps =
            [ prelude.unbounded "m4"
            , prelude.unbounded "automake"
            , prelude.unbounded "libtool"
            ]
          }

let nasm =
      λ(v : List Natural) →
        let versionString = "2.15.05"

        in    prelude.simplePackage { name = "nasm", version = v }
            ⫽ { pkgUrl =
                  "https://www.nasm.us/pub/nasm/releasebuilds/${versionString}/nasm-${versionString}.tar.xz"
              , pkgSubdir = "nasm-${versionString}"
              , installCommand =
                  λ(cfg : types.BuildVars) →
                      prelude.installWithBinaries
                        [ "bin/nasm", "bin/ndisasm" ]
                        cfg
                    # prelude.symlinkManpages
                        [ { file = "share/man/man1/nasm.1", section = 1 }
                        , { file = "share/man/man1/ndisasm.1", section = 1 }
                        ]
              }

let ncurses =
      λ(v : List Natural) →
          prelude.simplePackage { name = "ncurses", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/ncurses/ncurses-${prelude.showVersion
                                                           v}.tar.gz"
          , configureCommand =
              λ(cfg : types.BuildVars) →
                let crossArgs =
                      if    cfg.isCross
                      then  [ "--disable-stripping" ]
                      else  [] : List Text

                in  prelude.configureWithFlags
                      ([ "--with-shared", "--enable-widec" ] # crossArgs)
                      cfg
          , installCommand =
              λ(cfg : types.BuildVars) →
                  prelude.defaultInstall cfg
                # [ prelude.symlink "lib/libncursesw.so" "lib/libncurses.so" ]
          }

let pcre2 =
      λ(v : List Natural) →
          prelude.simplePackage { name = "pcre2", version = v }
        ⫽ { pkgUrl =
              "https://ftp.pcre.org/pub/pcre/pcre2-${prelude.showVersion
                                                       v}.tar.bz2"
          }

let pcre =
      λ(v : List Natural) →
          prelude.simplePackage { name = "pcre", version = v }
        ⫽ { pkgUrl =
              "https://ftp.pcre.org/pub/pcre/pcre-${prelude.showVersion
                                                      v}.tar.bz2"
          , configureCommand =
              prelude.configureWithFlags
                [ "--enable-utf8", "--enable-unicode-properties" ]
          }

let perl5 =
      let perlConfigure =
            λ(cfg : types.BuildVars) →
              [ prelude.call
                  (   prelude.defaultCall
                    ⫽ { program = "sh"
                      , arguments =
                            [ "./Configure"
                            , "-des"
                            , "-Dprefix=${cfg.installDir}"
                            ]
                          # ( if    cfg.static
                              then  [] : List Text
                              else  [ "-Duseshrplib" ]
                            )
                      }
                  )
              ]

      let linkDir =
            λ(os : types.OS) →
              merge
                { FreeBSD = "CORE"
                , OpenBSD = "CORE"
                , NetBSD = "CORE"
                , Solaris = "CORE"
                , Dragonfly = "CORE"
                , Linux = "CORE"
                , Darwin = "darwin-2level/CORE"
                , Windows = "CORE"
                , Redox = "CORE"
                , Haiku = "CORE"
                , IOS = "darwin-2level/CORE"
                , AIX = "CORE"
                , Hurd = "CORE"
                , Android = "CORE"
                , NoOs = "CORE"
                }
                os

      in  λ(v : List Natural) →
            let major =
                  merge { Some = Natural/show, None = "" } (List/head Natural v)

            in    prelude.simplePackage { name = "perl", version = v }
                ⫽ { pkgUrl =
                      "https://www.cpan.org/src/${major}.0/perl-${prelude.showVersion
                                                                    v}.tar.gz"
                  , configureCommand = perlConfigure
                  , buildCommand =
                      λ(cfg : types.BuildVars) →
                        prelude.generalBuild
                          prelude.singleThreaded
                          (prelude.buildEnv cfg)
                          cfg
                  , installCommand =
                      λ(cfg : types.BuildVars) →
                        let libperlFile =
                              if    cfg.static
                              then  "libperl.a"
                              else  "libperl.${prelude.libSuffix
                                                 (prelude.osCfg cfg)}"

                        in    prelude.installWithBinaries
                                [ "bin/perl", "bin/cpan", "bin/ptar" ]
                                cfg
                            # [ prelude.symlink
                                  "lib/${prelude.showVersion
                                           v}/${prelude.printArch
                                                  cfg.buildArch}-${prelude.printOS
                                                                     cfg.buildOS}/${linkDir
                                                                                      cfg.buildOS}/${libperlFile}"
                                  "lib/${libperlFile}"
                              ]
                  }

let libpng =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libpng", version = v }
        ⫽ { pkgUrl =
              "https://download.sourceforge.net/libpng/libpng-${prelude.showVersion
                                                                  v}.tar.xz"
          , pkgDeps = [ prelude.unbounded "zlib" ]
          }

let sed =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "sed", version = v }
        ⫽ { installCommand =
              prelude.installWithManpages
                [ { file = "share/man/man1/sed.1", section = 1 } ]
          , configureCommand =
              prelude.configureWithFlags [ "--disable-dependency-tracking" ]
          }

let tar = λ(v : List Natural) → prelude.makeGnuExe { name = "tar", version = v }

let unistring =
      λ(v : List Natural) →
        prelude.makeGnuLibrary { name = "unistring", version = v }

let valgrind =
      λ(v : List Natural) →
          prelude.simplePackage { name = "valgrind", version = v }
        ⫽ { pkgUrl =
              "https://sourceware.org/pub/valgrind/valgrind-${prelude.showVersion
                                                                v}.tar.bz2"
          , installCommand =
              λ(cfg : types.BuildVars) →
                  prelude.installWithBinaries [ "bin/valgrind" ] cfg
                # prelude.symlinkManpages
                    [ { file = "share/man/man1/pdfgrep.1", section = 1 } ]
          }

let vim =
      λ(v : List Natural) →
          prelude.defaultPackage
        ⫽ { pkgName = "vim"
          , pkgVersion = v
          , pkgUrl =
              "http://ftp.vim.org/vim/unix/vim-${prelude.showVersion v}.tar.bz2"
          , pkgSubdir = "vim${prelude.squishVersion v}"
          , configureCommand =
              prelude.configureMkExesExtraFlags
                { bins =
                  [ "src/configure", "src/auto/configure", "src/which.sh" ]
                , extraFlags = [ "--enable-gui=no", "--enable-pythoninterp" ]
                }
          , installCommand =
              λ(cfg : types.BuildVars) →
                let mkLibDynload =
                      λ(libs : List Text) →
                        concatMapSep
                          ":"
                          Text
                          ( λ(dir : Text) →
                              "${dir}:${dir}/python2.7/lib-dynload"
                          )
                          libs

                let mkPython =
                      λ(libs : List Text) →
                        concatMapSep
                          ":"
                          Text
                          ( λ(dir : Text) →
                              "${dir}/python2.7/:${dir}/python2.7/lib-dynload"
                          )
                          libs

                let wrapper =
                      "LD_LIBRARY_PATH=${mkLibDynload
                                           cfg.linkDirs} PYTHONPATH=${mkPython
                                                                        cfg.linkDirs} ${cfg.installDir}/bin/vim \$@"

                let wrapped = "wrapper/vim"

                in    prelude.installWithBinaries [ "bin/xxd" ] cfg
                    # [ prelude.createDir "wrapper"
                      , prelude.writeFile { file = wrapped, contents = wrapper }
                      , prelude.mkExe wrapped
                      , prelude.copyFile wrapped wrapped
                      , prelude.symlinkBinary wrapped
                      ]
          , pkgDeps =
            [ prelude.unbounded "ncurses"
            , prelude.unbounded "libXpm"
            , prelude.unbounded "libXt"
            , prelude.unbounded "python2"
            ]
          }

let xz =
      λ(v : List Natural) →
          prelude.simplePackage { name = "xz", version = v }
        ⫽ { pkgUrl = "https://tukaani.org/xz/xz-${prelude.showVersion v}.tar.xz"
          , installCommand = prelude.installWithBinaries [ "bin/xz" ]
          }

let zlib =
      λ(v : List Natural) →
        let zlibConfigure =
              λ(cfg : types.BuildVars) →
                let host = prelude.mkCCVar cfg

                in  [ prelude.call
                        (   prelude.defaultCall
                          ⫽ { program = "sh"
                            , arguments =
                              [ "configure", "--prefix=${cfg.installDir}" ]
                            , environment = Some
                                (   host
                                  # [ { var = "PATH"
                                      , value =
                                          "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
                                      }
                                    ]
                                )
                            }
                        )
                    ]

        in    prelude.simplePackage { name = "zlib", version = v }
            ⫽ { pkgUrl =
                  "http://www.zlib.net/zlib-${prelude.showVersion v}.tar.xz"
              , configureCommand = zlibConfigure
              , installCommand =
                  prelude.installWithManpages
                    [ { file = "share/man/man3/zlib.3", section = 3 } ]
              }

let gettext =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "gettext", version = v }
        ⫽ { installCommand =
              prelude.installWithBinaries
                [ "bin/gettext", "bin/msgfmt", "bin/autopoint" ]
          , configureCommand =
              prelude.configureWithFlags [ "--disable-dependency-tracking" ]
          }

let gzip =
      λ(v : List Natural) → prelude.makeGnuExe { name = "gzip", version = v }

let wget =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "wget", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/wget/wget-${prelude.showVersion
                                                     v}.tar.gz"
          , pkgDeps = [ prelude.unbounded "gnutls" ]
          , pkgBuildDeps = [ prelude.unbounded "perl" ]
          , installCommand = prelude.installWithWrappers [ "wget" ]
          }

let gnutls =
      λ(cfg : { version : List Natural, patch : List Natural }) →
        let versionString = prelude.showVersion cfg.version

        in    prelude.simplePackage
                { name = "gnutls", version = cfg.version # cfg.patch }
            ⫽ { pkgUrl =
                  "https://www.gnupg.org/ftp/gcrypt/gnutls/v${versionString}/gnutls-${versionString}.${prelude.showVersion
                                                                                                         cfg.patch}.tar.xz"
              , pkgDeps =
                [ prelude.lowerBound { name = "nettle", lower = [ 3, 1 ] }
                , prelude.unbounded "unistring"
                , prelude.lowerBound { name = "libtasn1", lower = [ 4, 9 ] }
                , prelude.lowerBound { name = "p11-kit", lower = [ 0, 23, 1 ] }
                , prelude.unbounded "guile"
                ]
              , configureCommand =
                  prelude.configureLinkExtraLibs [ "nettle", "hogweed" ]
              }

let lapack =
      λ(v : List Natural) →
          prelude.cmakePackage
        ⫽ { pkgName = "lapack"
          , pkgVersion = v
          , pkgUrl =
              "http://www.netlib.org/lapack/lapack-${prelude.showVersion
                                                       v}.tar.gz"
          , pkgSubdir = "lapack-${prelude.showVersion v}"
          , pkgBuildDeps =
            [ prelude.unbounded "cmake", prelude.unbounded "gcc" ]
          }

let cairo =
      let symlinkCairo =
            λ(h : Text) → prelude.symlink "include/cairo/${h}" "include/${h}"

      in  λ(v : List Natural) →
              prelude.simplePackage { name = "cairo", version = v }
            ⫽ { pkgUrl =
                  "https://www.cairographics.org/releases/cairo-${prelude.showVersion
                                                                    v}.tar.xz"
              , pkgDeps =
                [ prelude.lowerBound { name = "pixman", lower = [ 0, 30, 0 ] }
                , prelude.lowerBound { name = "freetype", lower = [ 9, 7, 3 ] }
                , prelude.lowerBound
                    { name = "fontconfig", lower = [ 2, 2, 95 ] }
                , prelude.unbounded "libXext"
                , prelude.unbounded "lzo"
                ]
              , installCommand =
                  λ(cfg : types.BuildVars) →
                      prelude.defaultInstall cfg
                    # [ symlinkCairo "cairo-deprecated.h"
                      , symlinkCairo "cairo-features.h"
                      , symlinkCairo "cairo-ft.h"
                      , symlinkCairo "cairo-gobject.h"
                      , symlinkCairo "cairo-pdf.h"
                      , symlinkCairo "cairo-ps.h"
                      , symlinkCairo "cairo-script-interpreter.h"
                      , symlinkCairo "cairo-script.h"
                      , symlinkCairo "cairo-svg.h"
                      , symlinkCairo "cairo-version.h"
                      , symlinkCairo "cairo-xcb.h"
                      , symlinkCairo "cairo-xlib-xrender.h"
                      , symlinkCairo "cairo-xlib.h"
                      , symlinkCairo "cairo.h"
                      ]
              }

let pycairo =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.python2Package { name = "pycairo", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/pygobject/pycairo/releases/download/v${versionString}/pycairo-${versionString}.tar.gz"
              , pkgDeps = [ prelude.unbounded "cairo" ]
              }

let libnettle =
      λ(v : List Natural) →
          prelude.simplePackage { name = "nettle", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/nettle/nettle-${prelude.showVersion
                                                         v}.tar.gz"
          , pkgBuildDeps = [ prelude.unbounded "m4" ]
          }

let diffutils =
      λ(v : List Natural) →
          prelude.simplePackage { name = "diffutils", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/diffutils/diffutils-${prelude.showVersion
                                                               v}.tar.xz"
          , installCommand = prelude.installWithBinaries [ "bin/diff" ]
          }

let patch =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "patch", version = v }
        ⫽ { installCommand =
              λ(cfg : types.BuildVars) →
                  prelude.installWithBinaries [ "bin/patch" ] cfg
                # prelude.symlinkManpages
                    [ { file = "share/man/man1/patch.1", section = 1 } ]
          }

let m4 =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "m4", version = v }
        ⫽ { configureCommand =
              λ(cfg : types.BuildVars) →
                  [ prelude.patch
                      "./m4.patch"
                  ]
                # prelude.defaultConfigure cfg
          , installCommand =
              prelude.installWithManpages
                [ { file = "share/man/man1/m4.1", section = 1 } ]
          , pkgBuildDeps = [ prelude.unbounded "patch" ]
          }

let nginx =
      λ(v : List Natural) →
          prelude.simplePackage { name = "nginx", version = v }
        ⫽ { pkgUrl =
              "http://nginx.org/download/nginx-${prelude.showVersion v}.tar.gz"
          , pkgDeps = [ prelude.unbounded "zlib", prelude.unbounded "pcre2" ]
          }

let openssl =
      let opensslCfgVars =
            λ(cfg : types.BuildVars) →
              Some
                (prelude.mkCCVar cfg # prelude.configEnv ([] : List Text) cfg)

      let cfgOS =
            λ(os : types.OS) →
              merge
                { FreeBSD = "BSD-x86_64"
                , OpenBSD = "BSD-x86_64"
                , NetBSD = "BSD-x86_64"
                , Solaris = "solaris64-x86_64-cc"
                , Dragonfly = "BSD-x86_64"
                , Linux = "linux-x86_64"
                , Darwin = "darwin64-x86_64-cc"
                , Windows = "mingw64"
                , Redox = "gcc"
                , Haiku = "haiku-x86_64"
                , IOS = "ios64-cross"
                , AIX = "aix64-cc"
                , Hurd = "hurd-x86"
                , Android = "android64-aarch64"
                , NoOs = "gcc"
                }
                os

      in  λ(v : List Natural) →
              prelude.simplePackage { name = "openssl", version = v }
            ⫽ { pkgUrl =
                  "https://www.openssl.org/source/openssl-${prelude.showVersion
                                                              v}d.tar.gz"
              , configureCommand =
                  λ(cfg : types.BuildVars) →
                    let sharedFlag =
                          if cfg.static then "no-shared" else "shared"

                    let targetMakefile =
                          if cfg.isCross then "gcc" else cfgOS cfg.buildOS

                    in  [ prelude.call
                            (   prelude.defaultCall
                              ⫽ { program = "perl"
                                , arguments =
                                  [ "./Configure"
                                  , "--prefix=${cfg.installDir}"
                                  , targetMakefile
                                  , sharedFlag
                                  ]
                                , environment = opensslCfgVars cfg
                                }
                            )
                        ]
              , pkgSubdir = "openssl-${prelude.showVersion v}d"
              , pkgBuildDeps = [ prelude.unbounded "perl" ]
              }

let libssh2 =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libssh2", version = v }
        ⫽ { pkgUrl =
              "https://www.libssh2.org/download/libssh2-${prelude.showVersion
                                                            v}.tar.gz"
          }

let giflib =
      λ(v : List Natural) →
          prelude.simplePackage { name = "giflib", version = v }
        ⫽ { pkgUrl =
              "https://downloads.sourceforge.net/giflib/giflib-${prelude.showVersion
                                                                   v}.tar.gz"
          , configureCommand = prelude.doNothing
          , installCommand =
              λ(cfg : types.BuildVars) →
                [ prelude.call
                    { program = "make"
                    , arguments = [ "PREFIX=${cfg.installDir}", "install" ]
                    , environment = Some (prelude.buildEnv cfg)
                    , procDir = None Text
                    }
                ]
          }

let emacs =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "emacs", version = v }
        ⫽ { pkgDeps =
            [ prelude.unbounded "giflib"
            , prelude.unbounded "libXaw"
            , prelude.unbounded "libpng"
            , prelude.unbounded "libjpeg-turbo"
            , prelude.unbounded "ncurses"
            , prelude.unbounded "gtk3"
            , prelude.unbounded "libotf"
            , prelude.unbounded "m17n-lib"
            , prelude.unbounded "gnutls"
            , prelude.unbounded "libXft"
            , prelude.unbounded "dbus"
            , prelude.unbounded "cairo"
            ]
          , configureCommand =
              prelude.configureMkExesExtraFlags
                { bins =
                  [ "build-aux/move-if-change", "build-aux/update-subdirs" ]
                , extraFlags =
                  [ "--with-tiff=no"
                  , "--with-libotf"
                  , "--with-m17n-flt"
                  , "--with-gnutls"
                  , "--with-xft"
                  , "--with-dbus"
                  , "--with-cairo=yes"
                  ]
                }
          , installCommand =
              λ(cfg : types.BuildVars) →
                if    cfg.static
                then  prelude.installWithBinaries [ "bin/emacs" ] cfg
                else  prelude.installWithWrappers [ "emacs" ] cfg
          }

let which =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "which", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/which/which-${prelude.showVersion
                                                       v}.tar.gz"
          }

let automake =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "automake", version = v }
        ⫽ { pkgBuildDeps =
            [ prelude.lowerBound { name = "autoconf", lower = [ 2, 65 ] } ]
          , installCommand =
              prelude.installWithBinaries [ "bin/automake", "bin/aclocal" ]
          }

let autoconf =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "autoconf", version = v }
        ⫽ { pkgBuildDeps =
            [ prelude.lowerBound { name = "m4", lower = [ 1, 4, 16 ] } ]
          , installCommand =
              prelude.installWithBinaries
                [ "bin/autoconf"
                , "bin/autoheader"
                , "bin/autom4te"
                , "bin/autoreconf"
                ]
          }

let python =
      λ(v : List Natural) →
        let major =
              merge { Some = Natural/show, None = "" } (List/head Natural v)

        let versionString = prelude.showVersion v

        let pyEnv =
              λ(_ : List Text) →
              λ(cfg : types.BuildVars) →
                Some
                  (   prelude.configEnv ([] : List Text) cfg
                    # [ { var = "CONFIG_SITE", value = "config.site" } ]
                  )

        in    prelude.simplePackage { name = "python${major}", version = v }
            ⫽ { pkgUrl =
                  "https://www.python.org/ftp/python/${versionString}/Python-${versionString}.tar.xz"
              , pkgSubdir = "Python-${versionString}"
              , configureCommand =
                  λ(cfg : types.BuildVars) →
                    let config =
                          ''
                          ac_cv_file__dev_ptmx=yes
                          ac_cv_file__dev_ptc=no
                          ''

                    let staticFlag =
                          if    cfg.static
                          then  [] : List Text
                          else  [ "--enable-shared" ]

                    let crossArgs =
                          if    cfg.isCross
                          then  [ "--disable-ipv6" ]
                          else  [] : List Text

                    in    [ prelude.writeFile
                              { file = "config.site", contents = config }
                          ]
                        # prelude.generalConfigure
                            pyEnv
                            "configure"
                            ([] : List Text)
                            (   [ "--build=${prelude.printArch cfg.buildArch}" ]
                              # crossArgs
                              # staticFlag
                            )
                            cfg
              , pkgDeps =
                [ prelude.unbounded "libffi", prelude.unbounded "ncurses" ]
              , installCommand =
                  λ(cfg : types.BuildVars) →
                      prelude.installWithBinaries [ "bin/python${major}" ] cfg
                    # prelude.symlinkManpages
                        [ { file = "share/man/man1/python${major}.1"
                          , section = 1
                          }
                        ]
              }

let lua =
      λ(v : List Natural) →
        let printLuaOS =
              λ(os : types.OS) →
                merge
                  { FreeBSD = "freebsd"
                  , OpenBSD = "bsd"
                  , NetBSD = "bsd"
                  , Solaris = "solaris"
                  , Dragonfly = "bsd"
                  , Linux = "linux"
                  , Darwin = "macosx"
                  , Windows = "mingw"
                  , Redox = "generic"
                  , Haiku = "generic"
                  , IOS = "generic"
                  , AIX = "generic"
                  , Hurd = "generic"
                  , Android = "generic"
                  , NoOs = "c89"
                  }
                  os

        let luaBuild =
              λ(cfg : types.BuildVars) →
                let cc = prelude.mkCCArg cfg

                let ldflags = (prelude.mkLDFlags cfg.linkDirs cfg).value

                let cflags = (prelude.mkCFlags cfg).value

                let os = prelude.osCfg cfg

                in  [ prelude.call
                        (   prelude.defaultCall
                          ⫽ { program = "make"
                            , arguments =
                                  cc
                                # [ printLuaOS os
                                  , "MYLDFLAGS=${ldflags}"
                                  , "MYCFLAGS=${cflags}"
                                  , "MYLIBS=-lncurses"
                                  , "-j${Natural/show cfg.cpus}"
                                  ]
                            }
                        )
                    ]

        let luaInstall =
              λ(cfg : types.BuildVars) →
                  [ prelude.call
                      (   prelude.defaultCall
                        ⫽ { program = "make"
                          , arguments =
                            [ "install", "INSTALL_TOP=${cfg.installDir}" ]
                          }
                      )
                  ]
                # prelude.symlinkBinaries [ "bin/lua", "bin/luac" ]

        in    prelude.simplePackage { name = "lua", version = v }
            ⫽ { pkgUrl =
                  "http://www.lua.org/ftp/lua-${prelude.showVersion v}.tar.gz"
              , configureCommand = prelude.doNothing
              , buildCommand = luaBuild
              , installCommand = luaInstall
              , pkgDeps =
                [ prelude.unbounded "readline", prelude.unbounded "ncurses" ]
              }

let libtasn1 =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libtasn1", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/libtasn1/libtasn1-${prelude.showVersion
                                                             v}.tar.gz"
          }

let p11kit =
      λ(v : List Natural) →
          prelude.simplePackage { name = "p11-kit", version = v }
        ⫽ { pkgUrl =
              let versionString = prelude.showVersion v

              in  "https://github.com/p11-glue/p11-kit/releases/download/${versionString}/p11-kit-${versionString}.tar.xz"
          , pkgDeps =
            [ prelude.lowerBound { name = "libffi", lower = [ 3, 0, 0 ] }
            , prelude.unbounded "libtasn1"
            ]
          , pkgBuildDeps = [ prelude.unbounded "pkg-config" ]
          }

let libffi =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "libffi", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/libffi/libffi/releases/download/v${versionString}/libffi-${versionString}.tar.gz"
              }

let gdb =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "gdb", version = v }
        ⫽ { pkgBuildDeps = [ prelude.unbounded "texinfo" ] }

let libtool =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "libtool", version = v }
        ⫽ { pkgUrl =
              "https://ftp.wayne.edu/gnu/libtool/libtool-${prelude.showVersion
                                                             v}.tar.xz"
          , pkgBuildDeps =
            [ prelude.lowerBound { name = "m4", lower = [ 1, 4, 16 ] } ]
          }

let pkg-config =
      λ(v : List Natural) →
          prelude.simplePackage { name = "pkg-config", version = v }
        ⫽ { pkgUrl =
              "https://pkg-config.freedesktop.org/releases/pkg-config-${prelude.showVersion
                                                                          v}.tar.gz"
          , configureCommand =
              prelude.configureWithFlags [ "--with-internal-glib" ]
          }

let qrencode =
      λ(v : List Natural) →
          prelude.simplePackage { name = "qrencode", version = v }
        ⫽ { pkgUrl =
              "https://fukuchi.org/works/qrencode/qrencode-${prelude.showVersion
                                                               v}.tar.gz"
          , configureCommand = prelude.configureWithFlags [ "--without-tools" ]
          }

let readline =
      λ(v : List Natural) →
          prelude.simplePackage { name = "readline", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/readline/readline-${prelude.showVersion
                                                             v}.tar.gz"
          , pkgBuildDeps = [ prelude.unbounded "make" ]
          }

let pixman =
      λ(v : List Natural) →
          prelude.simplePackage { name = "pixman", version = v }
        ⫽ { pkgUrl =
              "https://www.cairographics.org/releases/pixman-${prelude.showVersion
                                                                 v}.tar.gz"
          , pkgDeps = [ prelude.unbounded "libpng" ]
          }

let freetype-shared =
      λ(x : { name : Text, version : List Natural }) →
        let versionString = prelude.showVersion x.version

        in    prelude.simplePackage x
            ⫽ { pkgUrl =
                  "https://download.savannah.gnu.org/releases/freetype/freetype-${versionString}.tar.gz"
              , pkgSubdir = "freetype-${versionString}"
              , pkgBuildDeps = [ prelude.unbounded "sed" ]
              , installCommand =
                  λ(cfg : types.BuildVars) →
                      prelude.defaultInstall cfg
                    # [ prelude.symlink
                          "include/freetype2/ft2build.h"
                          "include/ft2build.h"
                      , prelude.symlink
                          "include/freetype2/freetype"
                          "include/freetype"
                      ]
              }

let freetype-prebuild =
      λ(v : List Natural) →
          freetype-shared { name = "freetype-prebuild", version = v }
        ⫽ { pkgDeps = [ prelude.unbounded "zlib", prelude.unbounded "libpng" ]
          , pkgBuildDeps = [ prelude.unbounded "pkg-config" ]
          }

let freetype =
      λ(v : List Natural) →
          freetype-shared { name = "freetype", version = v }
        ⫽ { pkgDeps =
            [ prelude.unbounded "zlib"
            , prelude.unbounded "harfbuzz"
            , prelude.unbounded "libpng"
            ]
          , configureCommand =
              prelude.configureMkExesExtraFlags
                { bins = [ "builds/unix/configure" ]
                , extraFlags = [ "--enable-freetype-config" ]
                }
          }

let sdl2 =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "sdl2", version = v }
            ⫽ { pkgUrl =
                  "https://www.libsdl.org/release/SDL2-${versionString}.tar.gz"
              , pkgSubdir = "SDL2-${versionString}"
              }

let imageMagick =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        let major =
              merge { Some = Natural/show, None = "" } (List/head Natural v)

        in    prelude.simplePackage { name = "imagemagick", version = v }
            ⫽ { pkgUrl =
                  "https://imagemagick.org/download/ImageMagick-${versionString}-67.tar.xz"
              , pkgSubdir = "ImageMagick-${versionString}-67"
              , pkgDeps =
                [ prelude.unbounded "zlib"
                , prelude.unbounded "libtool"
                , prelude.unbounded "bzip2"
                , prelude.unbounded "glib"
                ]
              , installCommand =
                  λ(cfg : types.BuildVars) →
                      prelude.defaultInstall cfg
                    # [ prelude.symlink
                          "include/ImageMagick-${major}/MagickWand"
                          "include/wand"
                      , prelude.symlinkBinary "bin/convert"
                      ]
              }

let gtk2 =
      let gtkEnv =
            λ(cfg : types.BuildVars) →
                prelude.defaultPath cfg
              # [ { var = "LDFLAGS"
                  , value =
                          (prelude.mkLDFlags cfg.linkDirs cfg).value
                      ++  " -lpcre -lfribidi"
                  }
                , prelude.mkCFlags cfg
                , prelude.mkPkgConfigVar cfg.linkDirs
                , prelude.libPath cfg
                , prelude.mkLDRunPath cfg.linkDirs
                , prelude.mkXdgDataDirs cfg.shareDirs
                , prelude.mkLDPreload cfg.preloadLibs
                ]

      let gtkConfig =
            λ(cfg : types.BuildVars) →
              [ prelude.call
                  (   prelude.defaultCall
                    ⫽ { program = "./configure"
                      , arguments = [ "--prefix=${cfg.installDir}" ]
                      , environment = Some (gtkEnv cfg)
                      }
                  )
              ]

      in  λ(x : { version : List Natural, patch : Natural }) →
            let versionString = prelude.showVersion x.version

            let fullVersion = versionString ++ "." ++ Natural/show x.patch

            in    prelude.simplePackage
                    { name = "gtk2", version = prelude.fullVersion x }
                ⫽ { pkgUrl =
                      "http://ftp.gnome.org/pub/gnome/sources/gtk+/${versionString}/gtk+-${fullVersion}.tar.xz"
                  , pkgSubdir = "gtk+-${fullVersion}"
                  , pkgDeps =
                    [ prelude.lowerBound { name = "cairo", lower = [ 1, 6 ] }
                    , prelude.lowerBound { name = "pango", lower = [ 1, 20 ] }
                    , prelude.lowerBound { name = "atk", lower = [ 1, 29, 2 ] }
                    , prelude.lowerBound { name = "glib", lower = [ 2, 28, 0 ] }
                    , prelude.lowerBound
                        { name = "gdk-pixbuf", lower = [ 2, 38, 0 ] }
                    ]
                  , buildCommand =
                      λ(cfg : types.BuildVars) →
                        prelude.buildWith (gtkEnv cfg) cfg
                  , configureCommand = gtkConfig
                  , installCommand =
                      λ(cfg : types.BuildVars) →
                          prelude.defaultInstall cfg
                        # [ prelude.symlink
                              "include/gdk-pixbuf-2.0/gdk-pixbuf"
                              "include/gdk-pixbuf"
                          ]
                  }

let mkXProto =
      λ(name : Text) →
      λ(v : List Natural) →
          prelude.simplePackage { name, version = v }
        ⫽ { pkgUrl =
              "https://www.x.org/releases/individual/proto/${name}-${prelude.showVersion
                                                                       v}.tar.bz2"
          }

let mkXProtoWithPatch =
      λ(name : Text) →
      λ(patch : Text) →
      λ(v : List Natural) →
          mkXProto name v
        ⫽ { configureCommand = prelude.configureWithPatch patch
          , pkgBuildDeps = [ prelude.unbounded "patch" ]
          }

let xproto =
      mkXProtoWithPatch
        "xproto"
        "./xproto.patch"

let renderproto =
      mkXProtoWithPatch
        "renderproto"
        "./renderproto.patch"

let randrproto = mkXProto "randrproto"

let scrnsaverproto =
      mkXProtoWithPatch
        "scrnsaverproto"
        "./scrnsaverproto.patch"

let recordproto = mkXProto "recordproto"

let xf86vidmodeproto = mkXProto "xf86vidmodeproto"

let glproto = mkXProto "glproto"

let dri2proto = mkXProto "dri2proto"

let pango =
      λ(x : { version : List Natural, patch : Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString ++ "." ++ Natural/show x.patch

        let pangoCfgFile =
              ''
              option('enable_docs',
                    description: 'Build API reference for Pango using GTK-Doc',
                    type: 'boolean',
                    value: false)
              option('gir',
                    description: 'Build the GObject introspection data for Pango',
                    type: 'boolean',
                    value: false)
              ''

        let no_gir =
              λ(cfg : types.BuildVars) →
                if    cfg.isCross
                then  [ prelude.writeFile
                          { file = "meson_options.txt"
                          , contents = pangoCfgFile
                          }
                      ]
                else  [] : List types.Command

        in    prelude.simplePackage
                { name = "pango", version = prelude.fullVersion x }
            ⫽ { pkgUrl =
                  "http://ftp.gnome.org/pub/GNOME/sources/pango/${versionString}/pango-${fullVersion}.tar.xz"
              , configureCommand =
                  λ(cfg : types.BuildVars) →
                    no_gir cfg # prelude.mesonConfigure cfg
              , buildCommand = prelude.ninjaBuild
              , installCommand =
                  λ(cfg : types.BuildVars) →
                      prelude.ninjaInstallWithPkgConfig
                        ( prelude.mesonMoves
                            [ "pango.pc", "pangocairo.pc", "pangoft2.pc" ]
                        )
                        cfg
                    # [ prelude.symlink
                          "include/pango-1.0/pango"
                          "include/pango"
                      ]
              , pkgBuildDeps =
                [ prelude.lowerBound { name = "meson", lower = [ 0, 48, 0 ] }
                , prelude.unbounded "gobject-introspection"
                ]
              , pkgDeps =
                [ prelude.lowerBound
                    { name = "fontconfig", lower = [ 2, 11, 91 ] }
                , prelude.lowerBound { name = "cairo", lower = [ 1, 12, 10 ] }
                , prelude.lowerBound { name = "fribidi", lower = [ 0, 19, 7 ] }
                , prelude.lowerBound { name = "harfbuzz", lower = [ 1, 4, 2 ] }
                , prelude.unbounded "libXrender"
                , prelude.unbounded "libxcb"
                ]
              }

let libxml2 =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libxml2", version = v }
        ⫽ { pkgUrl =
              "http://xmlsoft.org/sources/libxml2-${prelude.showVersion
                                                      v}.tar.gz"
          , pkgDeps =
            [ prelude.unbounded "zlib"
            , prelude.unbounded "xz"
            , prelude.unbounded "python2"
            ]
          }

let shared-mime-info =
      λ(v : List Natural) →
          prelude.simplePackage { name = "shared-mime-info", version = v }
        ⫽ { pkgUrl =
              "http://freedesktop.org/~hadess/shared-mime-info-${prelude.showVersion
                                                                   v}.tar.xz"
          , buildCommand =
              λ(cfg : types.BuildVars) →
                prelude.generalBuild
                  prelude.singleThreaded
                  (prelude.buildEnv cfg)
                  cfg
          , installCommand =
              λ(cfg : types.BuildVars) →
                  prelude.defaultInstall cfg
                # [ prelude.symlink "share/pkgconfig" "lib/pkgconfig" ]
          , pkgDeps = [ prelude.unbounded "glib", prelude.unbounded "libxml2" ]
          , pkgBuildDeps =
            [ prelude.lowerBound { name = "intltool", lower = [ 0, 35, 0 ] }
            , prelude.unbounded "sed"
            , prelude.unbounded "gettext"
            ]
          }

let intltool =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "intltool", version = v }
            ⫽ { pkgUrl =
                  "https://launchpad.net/intltool/trunk/${versionString}/+download/intltool-${versionString}.tar.gz"
              , configureCommand =
                  λ(cfg : types.BuildVars) →
                    [ prelude.patch
                        "./intltool.patch"
                    , prelude.call
                        (   prelude.defaultCall
                          ⫽ { program = "./configure"
                            , arguments = [ "--prefix=${cfg.installDir}" ]
                            , environment = Some
                                (   prelude.defaultPath cfg
                                  # [ prelude.mkPerlLib
                                        { libDirs = cfg.linkDirs
                                        , perlVersion = [ 5, 30, 1 ]
                                        , cfg
                                        }
                                    ]
                                )
                            }
                        )
                    ]
              , pkgDeps = [ prelude.unbounded "XML-Parser" ]
              , pkgBuildDeps =
                [ prelude.lowerBound { name = "perl", lower = [ 5, 8, 1 ] } ]
              }

let gdk-pixbuf =
      λ(x : { version : List Natural, patch : Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString ++ "." ++ Natural/show x.patch

        let gdkInstall =
              λ(fs : List { src : Text, dest : Text }) →
              λ(cfg : types.BuildVars) →
                  [ prelude.call
                      (   prelude.defaultCall
                        ⫽ { program = "ninja"
                          , environment = Some
                            [ prelude.mkPkgConfigVar cfg.linkDirs
                            , { var = "PATH"
                              , value =
                                      prelude.mkPathVar cfg.binDirs
                                  ++  ":${cfg.currentDir}/gdk-pixbuf-${fullVersion}/build/gdk-pixbuf:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
                              }
                            , prelude.mkPy3Path cfg.linkDirs
                            , prelude.libPath cfg
                            , prelude.mkLDRunPath cfg.linkDirs
                            , prelude.mkLDFlags cfg.linkDirs cfg
                            , prelude.mkCFlags cfg
                            ]
                          , arguments = [ "install" ]
                          , procDir = Some "build"
                          }
                      )
                  , prelude.symlink
                      "include/gdk-pixbuf-2.0/gdk-pixbuf"
                      "include/gdk-pixbuf"
                  ]
                # prelude.copyFiles fs

        in    prelude.simplePackage
                { name = "gdk-pixbuf", version = prelude.fullVersion x }
            ⫽ { pkgUrl =
                  "http://ftp.gnome.org/pub/GNOME/sources/gdk-pixbuf/${versionString}/gdk-pixbuf-${fullVersion}.tar.xz"
              , configureCommand = prelude.mesonConfigure
              , buildCommand = prelude.ninjaBuild
              , installCommand =
                  gdkInstall (prelude.mesonMoves [ "gdk-pixbuf-2.0.pc" ])
              , pkgDeps =
                [ prelude.unbounded "glib"
                , prelude.unbounded "libjpeg-turbo"
                , prelude.unbounded "libpng"
                , prelude.unbounded "gobject-introspection"
                , prelude.unbounded "shared-mime-info"
                , prelude.unbounded "libX11"
                ]
              }

let xmlParser =
      λ(v : List Natural) →
          prelude.simplePackage { name = "XML-Parser", version = v }
        ⫽ { pkgUrl =
              "https://cpan.metacpan.org/authors/id/T/TO/TODDR/XML-Parser-${prelude.showVersion
                                                                              v}.tar.gz"
          , configureCommand = prelude.perlConfigure
          , pkgBuildDeps = [ prelude.unbounded "perl" ]
          , pkgDeps = [ prelude.unbounded "expat" ]
          }

let meson =
      λ(v : List Natural) →
          prelude.simplePackage { name = "meson", version = v }
        ⫽ { pkgUrl =
              "https://github.com/mesonbuild/meson/archive/${prelude.showVersion
                                                               v}.tar.gz"
          , configureCommand =
              λ(cfg : types.BuildVars) →
                prelude.python3Install cfg # prelude.mkPy3Wrapper "meson" cfg
          , buildCommand = prelude.doNothing
          , installCommand = prelude.doNothing
          , pkgDeps = [ prelude.unbounded "python3" ]
          }

let ninja =
      let ninjaConfigure =
            λ(cfg : types.BuildVars) →
              [ prelude.call
                  (   prelude.defaultCall
                    ⫽ { program = "./configure.py"
                      , arguments = [ "--bootstrap" ]
                      }
                  )
              ]

      let ninjaInstall =
            λ(cfg : types.BuildVars) →
              [ prelude.copyFile "ninja" "bin/ninja"
              , prelude.symlinkBinary "bin/ninja"
              ]

      in  λ(v : List Natural) →
              prelude.simplePackage { name = "ninja", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/ninja-build/ninja/archive/v${prelude.showVersion
                                                                     v}.tar.gz"
              , configureCommand = ninjaConfigure
              , buildCommand = prelude.doNothing
              , installCommand = ninjaInstall
              , pkgBuildDeps = [ prelude.unbounded "python3" ]
              }

let fontconfig =
      λ(v : List Natural) →
          prelude.simplePackage { name = "fontconfig", version = v }
        ⫽ { pkgUrl =
              "https://www.freedesktop.org/software/fontconfig/release/fontconfig-${prelude.showVersion
                                                                                      v}.tar.bz2"
          , pkgDeps =
            [ prelude.unbounded "freetype"
            , prelude.unbounded "expat"
            , prelude.unbounded "util-linux"
            ]
          , pkgBuildDeps = [ prelude.unbounded "gperf" ]
          }

let util-linux =
      λ(x : { version : List Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString

        in    prelude.simplePackage { name = "util-linux", version = x.version }
            ⫽ { pkgUrl =
                  "https://mirrors.edge.kernel.org/pub/linux/utils/util-linux/v${versionString}/util-linux-${fullVersion}.tar.xz"
              , configureCommand =
                  λ(cfg : types.BuildVars) →
                    let crossFlags =
                          if    cfg.isCross
                          then  [ "--disable-pylibmount", "--without-tinfo" ]
                          else  [] : List Text

                    in  prelude.configureWithFlags
                          (   [ "--disable-makeinstall-chown"
                              , "--disable-bash-completion"
                              ]
                            # crossFlags
                          )
                          cfg
              , pkgDeps =
                [ prelude.unbounded "ncurses", prelude.unbounded "pcre2" ]
              }

let fribidi =
      λ(v : List Natural) →
          prelude.simplePackage { name = "fribidi", version = v }
        ⫽ { pkgUrl =
              "https://github.com/fribidi/fribidi/releases/download/v${prelude.showVersion
                                                                         v}/fribidi-${prelude.showVersion
                                                                                        v}.tar.xz"
          }

let gobject-introspection =
      λ(x : { version : List Natural, patch : Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString ++ "." ++ Natural/show x.patch

        in    prelude.ninjaPackage
                { name = "gobject-introspection"
                , version = prelude.fullVersion x
                }
            ⫽ { pkgUrl =
                  "https://download.gnome.org/sources/gobject-introspection/${versionString}/gobject-introspection-${fullVersion}.tar.xz"
              , pkgBuildDeps =
                [ prelude.unbounded "meson"
                , prelude.unbounded "m4"
                , prelude.unbounded "bison"
                , prelude.unbounded "flex"
                , prelude.unbounded "pkg-config"
                ]
              , pkgDeps =
                [ prelude.lowerBound { name = "glib", lower = [ 2, 58, 0 ] } ]
              , installCommand =
                  λ(cfg : types.BuildVars) →
                      [ prelude.mkExe "build/tools/g-ir-scanner"
                      , prelude.copyFile
                          "build/gobject-introspection-1.0.pc"
                          "lib/pkgconfig/gobject-introspection-1.0.pc"
                      ]
                    # prelude.ninjaInstall cfg
              }

let flex =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        let flexEnv =
              λ(_ : List Text) →
              λ(cfg : types.BuildVars) →
                Some
                  (   prelude.configEnv ([] : List Text) cfg
                    # [ { var = "YFLAGS", value = "-Wno-error=yacc" } ]
                  )

        in    prelude.simplePackage { name = "flex", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/westes/flex/releases/download/v${versionString}/flex-${versionString}.tar.gz"
              , pkgBuildDeps =
                [ prelude.unbounded "m4", prelude.unbounded "bison" ]
              , configureCommand = prelude.configWithEnv flexEnv
              , installCommand = prelude.installWithBinaries [ "bin/flex" ]
              }

let glib =
      λ(x : { version : List Natural, patch : Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString ++ "." ++ Natural/show x.patch

        let glibConfigure =
              λ(cfg : types.BuildVars) →
                let crossArgs =
                      if    cfg.isCross
                      then  [ "--cross-file", "cross.txt" ]
                      else  [] : List Text

                in  [ prelude.createDir "build"
                    , prelude.writeFile
                        { file = "build/cross.txt"
                        , contents = prelude.mesonCfgFile cfg
                        }
                    , prelude.call
                        { program = "meson"
                        , arguments =
                              [ "--prefix=${cfg.installDir}"
                              , ".."
                              , "-Dselinux=disabled"
                              ]
                            # crossArgs
                        , environment = Some
                            (   [ prelude.mkPkgConfigVar cfg.linkDirs
                                , { var = "LDFLAGS"
                                  , value =
                                      (prelude.mkLDFlags cfg.linkDirs cfg).value
                                  }
                                , prelude.libPath cfg
                                , prelude.mkCFlags cfg
                                , prelude.mkPkgConfigVar cfg.linkDirs
                                ]
                              # prelude.defaultPath cfg
                            )
                        , procDir = Some "build"
                        }
                    ]

        let symlinkGio =
              λ(h : Text) →
                prelude.symlink "include/glib-2.0/gio/${h}" "include/gio/${h}"

        let symlinkGunix =
              λ(h : Text) →
                prelude.symlink
                  "include/gio-unix-2.0/gio/${h}"
                  "include/gio/${h}"

        in    prelude.simplePackage
                { name = "glib", version = prelude.fullVersion x }
            ⫽ { pkgUrl =
                  "http://ftp.gnome.org/pub/gnome/sources/glib/${versionString}/glib-${fullVersion}.tar.xz"
              , configureCommand = glibConfigure
              , buildCommand =
                  λ(cfg : types.BuildVars) →
                      prelude.ninjaBuild cfg
                    # prelude.mkExes
                        [ "build/gobject/glib-mkenums"
                        , "build/gobject/glib-genmarshal"
                        , "build/gio/gdbus-2.0/codegen/gdbus-codegen"
                        ]
              , installCommand =
                  λ(cfg : types.BuildVars) →
                      prelude.ninjaInstallWithPkgConfig
                        ( prelude.mesonMoves
                            [ "glib-2.0.pc"
                            , "gobject-2.0.pc"
                            , "gio-2.0.pc"
                            , "gio-unix-2.0.pc"
                            , "gmodule-no-export-2.0.pc"
                            , "gmodule-export-2.0.pc"
                            , "gmodule-2.0.pc"
                            , "gthread-2.0.pc"
                            ]
                        )
                        cfg
                    # [ prelude.symlink "include/glib-2.0/glib" "include/glib"
                      , prelude.symlink
                          "include/glib-2.0/gobject"
                          "include/gobject"
                      , prelude.symlink
                          "include/glib-2.0/glib.h"
                          "include/glib.h"
                      , prelude.symlink
                          "include/glib-2.0/glib-object.h"
                          "include/glib-object.h"
                      , prelude.symlink
                          "include/glib-2.0/glib-unix.h"
                          "include/glib-unix.h"
                      , prelude.symlink
                          "include/glib-2.0/gmodule.h"
                          "include/gmodule.h"
                      , symlinkGio "gaction.h"
                      , symlinkGio "gactiongroup.h"
                      , symlinkGio "gactiongroupexporter.h"
                      , symlinkGio "gactionmap.h"
                      , symlinkGio "gappinfo.h"
                      , symlinkGio "gapplication.h"
                      , symlinkGio "gapplicationcommandline.h"
                      , symlinkGio "gasyncinitable.h"
                      , symlinkGio "gasyncresult.h"
                      , symlinkGio "gbufferedinputstream.h"
                      , symlinkGio "gbufferedoutputstream.h"
                      , symlinkGio "gbytesicon.h"
                      , symlinkGio "gcancellable.h"
                      , symlinkGio "gcharsetconverter.h"
                      , symlinkGio "gcontenttype.h"
                      , symlinkGio "gconverter.h"
                      , symlinkGio "gconverterinputstream.h"
                      , symlinkGio "gconverteroutputstream.h"
                      , symlinkGio "gcredentials.h"
                      , symlinkGio "gdatagrambased.h"
                      , symlinkGio "gdatainputstream.h"
                      , symlinkGio "gdataoutputstream.h"
                      , symlinkGio "gdbusactiongroup.h"
                      , symlinkGio "gdbusaddress.h"
                      , symlinkGio "gdbusauthobserver.h"
                      , symlinkGio "gdbusconnection.h"
                      , symlinkGio "gdbuserror.h"
                      , symlinkGio "gdbusinterface.h"
                      , symlinkGio "gdbusinterfaceskeleton.h"
                      , symlinkGio "gdbusintrospection.h"
                      , symlinkGio "gdbusmenumodel.h"
                      , symlinkGio "gdbusmessage.h"
                      , symlinkGio "gdbusmethodinvocation.h"
                      , symlinkGio "gdbusnameowning.h"
                      , symlinkGio "gdbusnamewatching.h"
                      , symlinkGio "gdbusobject.h"
                      , symlinkGio "gdbusobjectmanager.h"
                      , symlinkGio "gdbusobjectmanagerclient.h"
                      , symlinkGio "gdbusobjectmanagerserver.h"
                      , symlinkGio "gdbusobjectproxy.h"
                      , symlinkGio "gdbusobjectskeleton.h"
                      , symlinkGio "gdbusproxy.h"
                      , symlinkGio "gdbusserver.h"
                      , symlinkGio "gdbusutils.h"
                      , symlinkGio "gdrive.h"
                      , symlinkGio "gdtlsclientconnection.h"
                      , symlinkGio "gdtlsconnection.h"
                      , symlinkGio "gdtlsserverconnection.h"
                      , symlinkGio "gemblem.h"
                      , symlinkGio "gemblemedicon.h"
                      , symlinkGio "gfile.h"
                      , symlinkGio "gfileattribute.h"
                      , symlinkGio "gfileenumerator.h"
                      , symlinkGio "gfileicon.h"
                      , symlinkGio "gfileinfo.h"
                      , symlinkGio "gfileinputstream.h"
                      , symlinkGio "gfileiostream.h"
                      , symlinkGio "gfilemonitor.h"
                      , symlinkGio "gfilenamecompleter.h"
                      , symlinkGio "gfileoutputstream.h"
                      , symlinkGio "gfilterinputstream.h"
                      , symlinkGio "gfilteroutputstream.h"
                      , symlinkGio "gicon.h"
                      , symlinkGio "ginetaddress.h"
                      , symlinkGio "ginetaddressmask.h"
                      , symlinkGio "ginetsocketaddress.h"
                      , symlinkGio "ginitable.h"
                      , symlinkGio "ginputstream.h"
                      , symlinkGio "gio-autocleanups.h"
                      , symlinkGio "gio.h"
                      , symlinkGio "gioenums.h"
                      , symlinkGio "gioenumtypes.h"
                      , symlinkGio "gioerror.h"
                      , symlinkGio "giomodule.h"
                      , symlinkGio "gioscheduler.h"
                      , symlinkGio "giostream.h"
                      , symlinkGio "giotypes.h"
                      , symlinkGio "glistmodel.h"
                      , symlinkGio "gliststore.h"
                      , symlinkGio "gloadableicon.h"
                      , symlinkGio "gmemoryinputstream.h"
                      , symlinkGio "gmemoryoutputstream.h"
                      , symlinkGio "gmenu.h"
                      , symlinkGio "gmenuexporter.h"
                      , symlinkGio "gmenumodel.h"
                      , symlinkGio "gmount.h"
                      , symlinkGio "gmountoperation.h"
                      , symlinkGio "gnativevolumemonitor.h"
                      , symlinkGio "gnetworkaddress.h"
                      , symlinkGio "gnetworking.h"
                      , symlinkGio "gnetworkmonitor.h"
                      , symlinkGio "gnetworkservice.h"
                      , symlinkGio "gnotification.h"
                      , symlinkGio "goutputstream.h"
                      , symlinkGio "gpermission.h"
                      , symlinkGio "gpollableinputstream.h"
                      , symlinkGio "gpollableoutputstream.h"
                      , symlinkGio "gpollableutils.h"
                      , symlinkGio "gpropertyaction.h"
                      , symlinkGio "gproxy.h"
                      , symlinkGio "gproxyaddress.h"
                      , symlinkGio "gproxyaddressenumerator.h"
                      , symlinkGio "gproxyresolver.h"
                      , symlinkGio "gremoteactiongroup.h"
                      , symlinkGio "gresolver.h"
                      , symlinkGio "gresource.h"
                      , symlinkGio "gseekable.h"
                      , symlinkGio "gsettings.h"
                      , symlinkGio "gsettingsbackend.h"
                      , symlinkGio "gsettingsschema.h"
                      , symlinkGio "gsimpleaction.h"
                      , symlinkGio "gsimpleactiongroup.h"
                      , symlinkGio "gsimpleasyncresult.h"
                      , symlinkGio "gsimpleiostream.h"
                      , symlinkGio "gsimplepermission.h"
                      , symlinkGio "gsimpleproxyresolver.h"
                      , symlinkGio "gsocket.h"
                      , symlinkGio "gsocketaddress.h"
                      , symlinkGio "gsocketaddressenumerator.h"
                      , symlinkGio "gsocketclient.h"
                      , symlinkGio "gsocketconnectable.h"
                      , symlinkGio "gsocketconnection.h"
                      , symlinkGio "gsocketcontrolmessage.h"
                      , symlinkGio "gsocketlistener.h"
                      , symlinkGio "gsocketservice.h"
                      , symlinkGio "gsrvtarget.h"
                      , symlinkGio "gsubprocess.h"
                      , symlinkGio "gsubprocesslauncher.h"
                      , symlinkGio "gtask.h"
                      , symlinkGio "gtcpconnection.h"
                      , symlinkGio "gtcpwrapperconnection.h"
                      , symlinkGio "gtestdbus.h"
                      , symlinkGio "gthemedicon.h"
                      , symlinkGio "gthreadedsocketservice.h"
                      , symlinkGio "gtlsbackend.h"
                      , symlinkGio "gtlscertificate.h"
                      , symlinkGio "gtlsclientconnection.h"
                      , symlinkGio "gtlsconnection.h"
                      , symlinkGio "gtlsdatabase.h"
                      , symlinkGio "gtlsfiledatabase.h"
                      , symlinkGio "gtlsinteraction.h"
                      , symlinkGio "gtlspassword.h"
                      , symlinkGio "gtlsserverconnection.h"
                      , symlinkGio "gvfs.h"
                      , symlinkGio "gvolume.h"
                      , symlinkGio "gvolumemonitor.h"
                      , symlinkGio "gzlibcompressor.h"
                      , symlinkGio "gzlibdecompressor.h"
                      , symlinkGunix "gdesktopappinfo.h"
                      , symlinkGunix "gfiledescriptorbased.h"
                      , symlinkGunix "gunixconnection.h"
                      , symlinkGunix "gunixcredentialsmessage.h"
                      , symlinkGunix "gunixfdlist.h"
                      , symlinkGunix "gunixfdmessage.h"
                      , symlinkGunix "gunixinputstream.h"
                      , symlinkGunix "gunixmounts.h"
                      , symlinkGunix "gunixoutputstream.h"
                      , symlinkGunix "gunixsocketaddress.h"
                      ]
              , pkgBuildDeps =
                [ prelude.unbounded "meson", prelude.unbounded "ninja" ]
              , pkgDeps =
                [ prelude.unbounded "util-linux"
                , prelude.unbounded "pcre"
                , prelude.unbounded "libffi"
                , prelude.unbounded "zlib"
                , prelude.unbounded "dbus"
                ]
              }

let atk =
      λ(x : { version : List Natural, patch : Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString ++ "." ++ Natural/show x.patch

        in    prelude.ninjaPackage
                { name = "atk", version = prelude.fullVersion x }
            ⫽ { pkgUrl =
                  "https://ftp.gnome.org/pub/gnome/sources/atk/${versionString}/atk-${fullVersion}.tar.xz"
              , pkgBuildDeps =
                [ prelude.unbounded "gobject-introspection"
                , prelude.unbounded "gettext"
                ]
              , pkgDeps = [ prelude.unbounded "glib" ]
              , installCommand =
                  prelude.ninjaInstallWithPkgConfig
                    [ { src = "build/meson-private/atk.pc"
                      , dest = "lib/pkgconfig/atk.pc"
                      }
                    ]
              }

let re2c =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "re2c", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/skvadrik/re2c/releases/download/${versionString}/re2c-${versionString}.tar.xz"
              }

let chickenScheme =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        let printChickenOS =
              λ(os : types.OS) →
                merge
                  { FreeBSD = "bsd"
                  , OpenBSD = "bsd"
                  , NetBSD = "bsd"
                  , Solaris = "solaris"
                  , Dragonfly = "bsd"
                  , Linux = "linux"
                  , Darwin = "macosx"
                  , Windows = "mingw"
                  , Haiku = "haiku"
                  , IOS = "ios"
                  , AIX = "aix"
                  , Hurd = "hurd"
                  , Android = "android"
                  , Redox = "error: no port for Redox OS"
                  , NoOs = "error: no port for no OS"
                  }
                  os

        let chickenBuild =
              λ(cfg : types.BuildVars) →
                let cc =
                      merge
                        { Some =
                            λ(tgt : types.TargetTriple) →
                              [ "C_COMPILER=${prelude.printTargetTriple
                                                tgt}-gcc"
                              ]
                        , None = [] : List Text
                        }
                        cfg.targetTriple

                let os = prelude.osCfg cfg

                in    [ prelude.call
                          (   prelude.defaultCall
                            ⫽ { program = prelude.makeExe cfg.buildOS
                              , arguments =
                                    cc
                                  # [ "PLATFORM=${printChickenOS os}"
                                    , "PREFIX=${cfg.installDir}"
                                    ]
                              }
                          )
                      , prelude.call
                          (   prelude.defaultCall
                            ⫽ { program = prelude.makeExe cfg.buildOS
                              , arguments =
                                    cc
                                  # [ "PLATFORM=${printChickenOS os}"
                                    , "PREFIX=${cfg.installDir}"
                                    , "install"
                                    ]
                              }
                          )
                      ]
                    # prelude.symlinkBinaries
                        [ "bin/csc", "bin/chicken-install", "bin/csi" ]

        in    prelude.simplePackage { name = "chicken-scheme", version = v }
            ⫽ { pkgUrl =
                  "https://code.call-cc.org/releases/${versionString}/chicken-${versionString}.tar.gz"
              , configureCommand = prelude.doNothing
              , buildCommand = chickenBuild
              , installCommand = prelude.doNothing
              , pkgSubdir = "chicken-${versionString}"
              }

let xcb-proto =
      λ(v : List Natural) →
          prelude.simplePackage { name = "xcb-proto", version = v }
        ⫽ { pkgUrl =
              "https://xorg.freedesktop.org/archive/individual/xcb/xcb-proto-${prelude.showVersion
                                                                                 v}.tar.bz2"
          }

let libxcb =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libxcb", version = v }
        ⫽ { pkgUrl =
              "https://xorg.freedesktop.org/archive/individual/xcb/libxcb-${prelude.showVersion
                                                                              v}.tar.bz2"
          , pkgDeps =
            [ prelude.lowerBound { name = "xcb-proto", lower = [ 1, 13 ] }
            , prelude.unbounded "libXau"
            , prelude.unbounded "libpthread-stubs"
            , prelude.unbounded "libXdmcp"
            ]
          }

let libpthread-stubs =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libpthread-stubs", version = v }
        ⫽ { pkgUrl =
              "https://www.x.org/archive/individual/xcb/libpthread-stubs-${prelude.showVersion
                                                                             v}.tar.bz2"
          }

let xorgConfigure =
      prelude.configureWithFlags [ "--disable-malloc0returnsnull" ]

let mkXLib =
      λ(name : Text) →
      λ(v : List Natural) →
          prelude.simplePackage { name, version = v }
        ⫽ { pkgUrl =
              "https://www.x.org/releases/individual/lib/${name}-${prelude.showVersion
                                                                     v}.tar.bz2"
          , configureCommand = xorgConfigure
          , pkgBuildDeps = [ prelude.unbounded "pkg-config" ]
          }

let mkXLibDeps =
      λ(x : { name : Text, deps : List types.Dep }) →
      λ(v : List Natural) →
        mkXLib x.name v ⫽ { pkgDeps = x.deps }

let libXdmcp =
      mkXLibDeps { name = "libXdmcp", deps = [ prelude.unbounded "xproto" ] }

let libXau =
      mkXLibDeps { name = "libXau", deps = [ prelude.unbounded "xproto" ] }

let mkXUtil =
      λ(name : Text) →
      λ(v : List Natural) →
          prelude.simplePackage { name, version = v }
        ⫽ { pkgUrl =
              "https://www.x.org/releases/individual/util/${name}-${prelude.showVersion
                                                                      v}.tar.bz2"
          }

let libXrender =
      mkXLibDeps
        { name = "libXrender"
        , deps =
          [ prelude.unbounded "xproto"
          , prelude.unbounded "renderproto"
          , prelude.unbounded "libX11"
          ]
        }

let util-macros = mkXUtil "util-macros"

let libXft =
      mkXLibDeps
        { name = "libXft"
        , deps =
          [ prelude.unbounded "freetype"
          , prelude.unbounded "fontconfig"
          , prelude.unbounded "libXrender"
          , prelude.unbounded "libX11"
          ]
        }

let kbproto = mkXProto "kbproto"

let libX11 =
      mkXLibDeps
        { name = "libX11"
        , deps =
          [ prelude.unbounded "libxcb"
          , prelude.unbounded "kbproto"
          , prelude.unbounded "xextproto"
          , prelude.unbounded "inputproto"
          , prelude.unbounded "xtrans"
          ]
        }

let inputproto = mkXProto "inputproto"

let xineramaproto =
      mkXProtoWithPatch
        "xineramaproto"
        "./xineramaproto.patch"

let xtrans = mkXLib "xtrans"

let libXrandr =
      mkXLibDeps
        { name = "libXrandr"
        , deps =
          [ prelude.unbounded "util-macros"
          , prelude.unbounded "libXext"
          , prelude.unbounded "libXrender"
          , prelude.unbounded "libX11"
          , prelude.unbounded "randrproto"
          ]
        }

let libXinerama =
      mkXLibDeps
        { name = "libXinerama"
        , deps =
          [ prelude.unbounded "util-macros"
          , prelude.unbounded "libX11"
          , prelude.unbounded "libXext"
          , prelude.unbounded "xineramaproto"
          ]
        }

let libXext =
      mkXLibDeps
        { name = "libXext"
        , deps =
          [ prelude.lowerBound { name = "xextproto", lower = [ 7, 1, 99 ] }
          , prelude.lowerBound { name = "xproto", lower = [ 7, 0, 13 ] }
          , prelude.lowerBound { name = "libX11", lower = [ 1, 6 ] }
          ]
        }

let xextproto =
      mkXProtoWithPatch
        "xextproto"
        "./xextproto.patch"

let fixesproto = mkXProto "fixesproto"

let damageproto = mkXProto "damageproto"

let libXScrnSaver =
      λ(v : List Natural) →
          mkXLib "libXScrnSaver" v
        ⫽ { pkgDeps =
            [ prelude.unbounded "util-macros"
            , prelude.unbounded "libXext"
            , prelude.unbounded "scrnsaverproto"
            ]
          }

let bzip2 =
      let cc = prelude.mkCCArg

      in  λ(v : List Natural) →
            let versionString = prelude.showVersion v

            let bzipInstall =
                  λ(cfg : types.BuildVars) →
                    [ prelude.call
                        (   prelude.defaultCall
                          ⫽ { program = prelude.makeExe cfg.buildOS
                            , arguments =
                                  cc cfg
                                # [ "PREFIX=${cfg.installDir}"
                                  , "install"
                                  , "-j${Natural/show cfg.cpus}"
                                  ]
                            }
                        )
                    , prelude.copyFile
                        "libbz2.so.${versionString}"
                        "lib/libbz2.so.${versionString}"
                    , prelude.symlink
                        "lib/libbz2.so.${versionString}"
                        "lib/libbz2.so.1.0"
                    , prelude.symlink
                        "lib/libbz2.so.${versionString}"
                        "lib/libbz2.so"
                    , prelude.symlinkBinary "bin/bzip2"
                    , prelude.symlinkBinary "bin/bzip2recover"
                    ]

            let bzipShared =
                  λ(cfg : types.BuildVars) →
                    [ prelude.call
                        (   prelude.defaultCall
                          ⫽ { program = prelude.makeExe cfg.buildOS
                            , arguments =
                                cc cfg # [ "-f", "Makefile-libbz2_so" ]
                            }
                        )
                    ]

            in    prelude.simplePackage { name = "bzip2", version = v }
                ⫽ { pkgUrl =
                      "https://www.sourceware.org/pub/bzip2/bzip2-${versionString}.tar.gz"
                  , configureCommand = prelude.doNothing
                  , buildCommand = bzipShared
                  , installCommand = bzipInstall
                  }

let expat =
      λ(v : List Natural) →
          prelude.simplePackage { name = "expat", version = v }
        ⫽ { pkgUrl =
              "https://github.com/libexpat/libexpat/releases/download/R_${prelude.underscoreVersion
                                                                            v}/expat-${prelude.showVersion
                                                                                         v}.tar.bz2"
          }

let gperf =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "gperf", version = v }
        ⫽ { pkgUrl =
              "http://ftp.gnu.org/pub/gnu/gperf/gperf-${prelude.showVersion
                                                          v}.tar.gz"
          }

let coreutils =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "coreutils", version = v }
        ⫽ { installCommand =
              prelude.installWithBinaries
                [ "bin/install"
                , "bin/chmod"
                , "bin/rm"
                , "bin/cp"
                , "bin/ln"
                , "bin/mkdir"
                , "bin/test"
                , "bin/od"
                , "bin/readlink"
                ]
          }

let libsepol =
      let cc = prelude.mkCCArg

      let sepolInstall =
            λ(cfg : types.BuildVars) →
              [ prelude.call
                  (   prelude.defaultCall
                    ⫽ { program = prelude.makeExe cfg.buildOS
                      , arguments =
                            cc cfg
                          # [ "PREFIX=${cfg.installDir}"
                            , "SHLIBDIR=${cfg.installDir}/lib"
                            , "CFLAGS=-Wno-error -O2"
                            , "install"
                            , "-j${Natural/show cfg.cpus}"
                            ]
                      , environment = Some
                          (   prelude.defaultPath cfg
                            # [ prelude.mkLDFlags cfg.linkDirs cfg
                              , prelude.mkCFlags cfg
                              , prelude.mkPkgConfigVar cfg.linkDirs
                              ]
                          )
                      }
                  )
              ]

      in  λ(v : List Natural) →
              prelude.simplePackage { name = "libsepol", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/SELinuxProject/selinux/releases/download/20191204/libsepol-${prelude.showVersion
                                                                                                     v}.tar.gz"
              , configureCommand = prelude.doNothing
              , buildCommand = prelude.doNothing
              , installCommand = sepolInstall
              , pkgBuildDeps = [ prelude.unbounded "flex" ]
              }

let libselinux =
      let cc = prelude.mkCCArg

      let selinuxInstall =
            λ(cfg : types.BuildVars) →
              [ prelude.call
                  (   prelude.defaultCall
                    ⫽ { program = prelude.makeExe cfg.buildOS
                      , arguments =
                            cc cfg
                          # [ "PREFIX=${cfg.installDir}"
                            , "SHLIBDIR=${cfg.installDir}/lib"
                            ,     "EXTRA_CFLAGS=-Wno-error -lpcre "
                              ++  (prelude.mkCFlags cfg).value
                            , "install"
                            , "-j${Natural/show cfg.cpus}"
                            ]
                      , environment = Some
                          (   prelude.defaultPath cfg
                            # [ prelude.mkLDFlags cfg.linkDirs cfg
                              , prelude.mkCFlags cfg
                              , prelude.mkPkgConfigVar cfg.linkDirs
                              , prelude.libPath cfg
                              ]
                          )
                      }
                  )
              ]

      in  λ(v : List Natural) →
              prelude.simplePackage { name = "libselinux", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/SELinuxProject/selinux/releases/download/20191204/libselinux-${prelude.showVersion
                                                                                                       v}.tar.gz"
              , configureCommand = prelude.doNothing
              , buildCommand = prelude.doNothing
              , installCommand = selinuxInstall
              , pkgDeps =
                [ prelude.unbounded "pcre", prelude.unbounded "libsepol" ]
              , pkgBuildDeps = [ prelude.unbounded "pkg-config" ]
              }

let libXtst =
      mkXLibDeps
        { name = "libXtst"
        , deps = [ prelude.unbounded "libXi", prelude.unbounded "recordproto" ]
        }

let libXi =
      mkXLibDeps
        { name = "libXi"
        , deps = [ prelude.unbounded "libXext", prelude.unbounded "libXfixes" ]
        }

let mkGnomeNinja =
      λ(name : Text) →
      λ(x : { version : List Natural, patch : Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString ++ "." ++ Natural/show x.patch

        in    prelude.ninjaPackage { name, version = prelude.fullVersion x }
            ⫽ { pkgUrl =
                  "http://ftp.gnome.org/pub/gnome/sources/${name}/${versionString}/${name}-${fullVersion}.tar.xz"
              }

let at-spi-core =
      λ(x : { version : List Natural, patch : Natural }) →
          mkGnomeNinja "at-spi2-core" x
        ⫽ { pkgDeps = [ prelude.unbounded "libXtst", prelude.unbounded "glib" ]
          , installCommand =
              prelude.ninjaInstallWithPkgConfig
                [ { src = "build/meson-private/atspi-2.pc"
                  , dest = "lib/pkgconfig/atspi-2.pc"
                  }
                ]
          }

let at-spi-atk =
      λ(x : { version : List Natural, patch : Natural }) →
          mkGnomeNinja "at-spi2-atk" x
        ⫽ { pkgDeps =
            [ prelude.lowerBound { name = "at-spi2-core", lower = [ 2, 32, 2 ] }
            , prelude.lowerBound { name = "atk", lower = [ 2, 29, 2 ] }
            , prelude.unbounded "libxml2"
            ]
          , installCommand =
              prelude.ninjaInstallWithPkgConfig
                (prelude.mesonMoves [ "atk-bridge-2.0.pc" ])
          }

let libdrm =
      λ(v : List Natural) →
          prelude.ninjaPackage { name = "libdrm", version = v }
        ⫽ { pkgUrl =
              "https://dri.freedesktop.org/libdrm/libdrm-${prelude.showVersion
                                                             v}.tar.xz"
          , pkgDeps =
            [ prelude.unbounded "libpciaccess", prelude.unbounded "cairo" ]
          , installCommand =
              prelude.ninjaInstallWithPkgConfig
                ( prelude.mesonMoves
                    [ "libdrm.pc"
                    , "libdrm_amdgpu.pc"
                    , "libdrm_intel.pc"
                    , "libdrm_nouveau.pc"
                    , "libdrm_radeon.pc"
                    , "libkms.pc"
                    ]
                )
          }

let libpciaccess = mkXLib "libpciaccess"

let markupSafe =
      λ(v : List Natural) →
          prelude.python3Package { name = "MarkupSafe", version = v }
        ⫽ { pkgUrl =
              "https://files.pythonhosted.org/packages/source/M/MarkupSafe/MarkupSafe-${prelude.showVersion
                                                                                          v}.tar.gz"
          }

let mako =
      λ(v : List Natural) →
          prelude.python3Package { name = "Mako", version = v }
        ⫽ { pkgUrl =
              "https://files.pythonhosted.org/packages/source/M/Mako/Mako-${prelude.showVersion
                                                                              v}.tar.gz"
          }

let elfutils =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "elfutils", version = v }
            ⫽ { pkgUrl =
                  "https://sourceware.org/ftp/elfutils/${versionString}/elfutils-${versionString}.tar.bz2"
              }

let mkGnomeSimple =
      λ(name : Text) →
      λ(x : { version : List Natural, patch : Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString ++ "." ++ Natural/show x.patch

        in    prelude.simplePackage { name, version = prelude.fullVersion x }
            ⫽ { pkgUrl =
                  "http://ftp.gnome.org/pub/gnome/sources/${name}/${versionString}/${name}-${fullVersion}.tar.xz"
              }

let gtk3 =
      let mkLDFlagsGtk =
            λ(linkDirs : List Text) →
              concatMapSep " " Text (λ(dir : Text) → "-L${dir}") linkDirs

      let gtkEnv =
            λ(cfg : types.BuildVars) →
                prelude.defaultPath cfg
              # [ { var = "LDFLAGS"
                  , value = mkLDFlagsGtk cfg.linkDirs ++ " -lpcre -lfribidi"
                  }
                , prelude.mkPkgConfigVar cfg.linkDirs
                , prelude.mkLDPreload cfg.preloadLibs
                , prelude.mkXdgDataDirs cfg.shareDirs
                , prelude.mkCFlags cfg
                ]

      let gtkConfig =
            λ(cfg : types.BuildVars) →
              [ prelude.call
                  (   prelude.defaultCall
                    ⫽ { program = "./configure"
                      , arguments = [ "--prefix=${cfg.installDir}" ]
                      , environment = Some (gtkEnv cfg)
                      }
                  )
              ]

      in  λ(x : { version : List Natural, patch : Natural }) →
              mkGnomeSimple "gtk+" x
            ⫽ { pkgName = "gtk3"
              , configureCommand = gtkConfig
              , buildCommand =
                  λ(cfg : types.BuildVars) → prelude.buildWith (gtkEnv cfg) cfg
              , pkgDeps =
                [ prelude.lowerBound { name = "pango", lower = [ 1, 41, 0 ] }
                , prelude.unbounded "at-spi2-atk"
                , prelude.lowerBound { name = "atk", lower = [ 2, 15, 1 ] }
                , prelude.lowerBound
                    { name = "gdk-pixbuf", lower = [ 2, 30, 0 ] }
                , prelude.unbounded "libXft"
                , prelude.lowerBound { name = "libepoxy", lower = [ 1, 4 ] }
                , prelude.unbounded "libXi"
                ]
              , pkgBuildDeps = [ prelude.unbounded "binutils" ]
              }

let graphviz =
      λ(v : List Natural) →
          prelude.simplePackage { name = "graphviz", version = v }
        ⫽ { pkgUrl =
              "https://www2.graphviz.org/Packages/stable/portable_source/graphviz-${prelude.showVersion
                                                                                      v}.tar.gz"
          , pkgDeps = [ prelude.unbounded "perl" ]
          , installCommand =
              λ(cfg : types.BuildVars) →
                  prelude.installWithBinaries [ "bin/dot" ] cfg
                # prelude.symlinkManpages
                    [ { file = "share/man/man1/dot.1", section = 1 } ]
          }

let wayland =
      λ(v : List Natural) →
          prelude.simplePackage { name = "wayland", version = v }
        ⫽ { pkgUrl =
              "https://wayland.freedesktop.org/releases/wayland-${prelude.showVersion
                                                                    v}.tar.xz"
          , pkgDeps = [ prelude.unbounded "libxml2" ]
          , configureCommand =
              prelude.configureWithFlags [ "--disable-documentation" ]
          }

let swig =
      λ(v : List Natural) →
          prelude.simplePackage { name = "swig", version = v }
        ⫽ { pkgUrl =
              "https://downloads.sourceforge.net/swig/swig-${prelude.showVersion
                                                               v}.tar.gz"
          , installCommand = prelude.installWithBinaries [ "bin/swig" ]
          }

let lmdb =
      let cc = prelude.mkCCArg

      let ar =
            λ(cfg : types.BuildVars) →
              merge
                { Some =
                    λ(tgt : types.TargetTriple) →
                      [ "AR=${prelude.printTargetTriple tgt}-ar" ]
                , None = [] : List Text
                }
                cfg.targetTriple

      let lmdbInstall =
            λ(cfg : types.BuildVars) →
              [ prelude.call
                  (   prelude.defaultCall
                    ⫽ { program = "make"
                      , arguments =
                            cc cfg
                          # ar cfg
                          # [ "prefix=${cfg.installDir}"
                            , "install"
                            , "-j${Natural/show cfg.cpus}"
                            ]
                      , procDir = Some "libraries/liblmdb"
                      }
                  )
              ]

      in  λ(v : List Natural) →
            let versionString = prelude.showVersion v

            in    prelude.simplePackage { name = "lmdb", version = v }
                ⫽ { pkgUrl =
                      "https://github.com/LMDB/lmdb/archive/LMDB_${versionString}.tar.gz"
                  , pkgSubdir = "lmdb-LMDB_${versionString}"
                  , configureCommand = prelude.doNothing
                  , buildCommand = prelude.doNothing
                  , installCommand = lmdbInstall
                  }

let gsl =
      λ(v : List Natural) →
          prelude.simplePackage { name = "gsl", version = v }
        ⫽ { pkgUrl =
              "http://mirror.keystealth.org/gnu/gsl/gsl-${prelude.showVersion
                                                            v}.tar.gz"
          }

let postgresql =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "postgresql", version = v }
            ⫽ { pkgUrl =
                  "https://ftp.postgresql.org/pub/source/v${versionString}/postgresql-${versionString}.tar.bz2"
              , configureCommand =
                  prelude.configureWithFlags [ "--without-readline" ]
              , installCommand = prelude.installWithBinaries [ "bin/pg_config" ]
              , pkgDeps = [ prelude.unbounded "zlib" ]
              }

let sqlite =
      λ(x : { version : List Natural }) →
          prelude.simplePackage { name = "sqlite", version = x.version }
        ⫽ { pkgUrl =
              "https://www.sqlite.org/2019/sqlite-autoconf-3300100.tar.gz"
          , pkgSubdir = "sqlite-autoconf-3300100"
          }

let ragel =
      λ(v : List Natural) →
          prelude.simplePackage { name = "ragel", version = v }
        ⫽ { pkgUrl =
              "http://www.colm.net/files/ragel/ragel-${prelude.showVersion
                                                         v}.tar.gz"
          , installCommand = prelude.installWithBinaries [ "bin/ragel" ]
          }

let nano =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "nano", version = v }
        ⫽ { pkgDeps = [ prelude.unbounded "ncurses" ] }

let libarchive =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libarchive", version = v }
        ⫽ { pkgUrl =
              "https://www.libarchive.org/downloads/libarchive-${prelude.showVersion
                                                                   v}.tar.gz"
          , pkgDeps =
            [ prelude.unbounded "xz"
            , prelude.unbounded "bzip2"
            , prelude.unbounded "zlib"
            ]
          , installCommand =
              prelude.installWithBinaries [ "bin/bsdtar", "bin/bsdcpio" ]
          }

let pygobject =
      λ(x : { version : List Natural, patch : Natural }) →
          mkGnomeSimple "pygobject" x
        ⫽ { pkgDeps = [ prelude.unbounded "glib" ]
          , configureCommand = prelude.preloadCfg
          }

let pygtk =
      λ(x : { version : List Natural, patch : Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString ++ "." ++ Natural/show x.patch

        in    mkGnomeSimple "pygtk" x
            ⫽ { pkgUrl =
                  "http://ftp.gnome.org/pub/gnome/sources/pygtk/${versionString}/pygtk-${fullVersion}.tar.bz2"
              , configureCommand = prelude.preloadCfg
              , pkgDeps =
                [ prelude.lowerBound { name = "glib", lower = [ 2, 8, 0 ] }
                , prelude.lowerBound
                    { name = "pygobject", lower = [ 2, 21, 3 ] }
                , prelude.unbounded "python2"
                ]
              }

let libglade =
      λ(x : { version : List Natural, patch : Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString ++ "." ++ Natural/show x.patch

        in    prelude.simplePackage
                { name = "libglade", version = prelude.fullVersion x }
            ⫽ { pkgUrl =
                  "http://ftp.gnome.org/pub/gnome/sources/libglade/${versionString}/libglade-${fullVersion}.tar.bz2"
              , pkgDeps =
                [ prelude.lowerBound { name = "libxml2", lower = [ 2, 4, 10 ] }
                , prelude.lowerBound { name = "gtk2", lower = [ 2, 5, 0 ] }
                ]
              , configureCommand = prelude.configureLinkExtraLibs [ "fribidi" ]
              }

let scour =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.python3Package { name = "scour", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/scour-project/scour/archive/v${versionString}/scour-${versionString}.tar.gz"
              , installCommand = prelude.installWithPy3Wrappers [ "scour" ]
              }

let libXpm =
      λ(v : List Natural) →
          mkXLib "libXpm" v
        ⫽ { pkgDeps = [ prelude.unbounded "libXext", prelude.unbounded "libXt" ]
          , pkgBuildDeps = [ prelude.unbounded "gettext" ]
          }

let libXt =
      mkXLibDeps
        { name = "libXt"
        , deps =
          [ prelude.unbounded "libICE"
          , prelude.unbounded "libSM"
          , prelude.unbounded "libX11"
          , prelude.unbounded "kbproto"
          ]
        }

let libICE =
      mkXLibDeps
        { name = "libICE"
        , deps = [ prelude.unbounded "xproto", prelude.unbounded "xtrans" ]
        }

let libSM =
      mkXLibDeps
        { name = "libSM"
        , deps = [ prelude.unbounded "libICE", prelude.unbounded "util-linux" ]
        }

let libXaw =
      mkXLibDeps
        { name = "libXaw"
        , deps = [ prelude.unbounded "libXmu", prelude.unbounded "libXpm" ]
        }

let libXaw3d =
      mkXLibDeps
        { name = "libXaw3d"
        , deps =
          [ prelude.unbounded "libX11"
          , prelude.unbounded "libXt"
          , prelude.unbounded "libXmu"
          , prelude.unbounded "libXext"
          ]
        }

let libXmu =
      mkXLibDeps
        { name = "libXmu"
        , deps =
          [ prelude.unbounded "util-macros"
          , prelude.unbounded "libXt"
          , prelude.unbounded "libXext"
          ]
        }

let libotf =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libotf", version = v }
        ⫽ { pkgUrl =
              "http://download.savannah.gnu.org/releases/m17n/libotf-${prelude.showVersion
                                                                         v}.tar.gz"
          , pkgDeps = [ prelude.unbounded "freetype" ]
          }

let m17n =
      λ(v : List Natural) →
          prelude.simplePackage { name = "m17n-lib", version = v }
        ⫽ { pkgUrl =
              "http://download.savannah.gnu.org/releases/m17n/m17n-lib-${prelude.showVersion
                                                                           v}.tar.gz"
          , buildCommand =
              λ(cfg : types.BuildVars) →
                prelude.generalBuild
                  prelude.singleThreaded
                  (prelude.buildEnv cfg)
                  cfg
          , pkgDeps = [ prelude.unbounded "libXt" ]
          , pkgBuildDeps = [ prelude.unbounded "binutils" ]
          }

let mkGimpPackage =
      λ(name : Text) →
      λ(x : { version : List Natural, patch : Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString ++ "." ++ Natural/show x.patch

        in    prelude.simplePackage { name, version = prelude.fullVersion x }
            ⫽ { pkgUrl =
                  "https://download.gimp.org/pub/${name}/${versionString}/${name}-${fullVersion}.tar.bz2"
              }

let babl =
      λ(x : { version : List Natural, patch : Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString ++ "." ++ Natural/show x.patch

        in    prelude.simplePackage
                { name = "babl", version = prelude.fullVersion x }
            ⫽ { pkgUrl =
                  "https://download.gimp.org/pub/babl/${versionString}/babl-${fullVersion}.tar.xz"
              , pkgBuildDeps =
                [ prelude.unbounded "autoconf"
                , prelude.unbounded "automake"
                , prelude.unbounded "libtool"
                , prelude.unbounded "pkg-config"
                ]
              , pkgDeps = [ prelude.unbounded "lcms2" ]
              , configureCommand = prelude.autogenConfigure
              }

let gegl =
      λ(x : { version : List Natural, patch : Natural }) →
          mkGimpPackage "gegl" x
        ⫽ { pkgDeps =
            [ prelude.lowerBound { name = "babl", lower = [ 0, 1, 58 ] }
            , prelude.lowerBound { name = "glib", lower = [ 2, 44, 0 ] }
            , prelude.unbounded "json-glib"
            ]
          , configureCommand = prelude.preloadCfg
          }

let libexif =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "libexif", version = v }
            ⫽ { pkgUrl =
                  "https://nchc.dl.sourceforge.net/project/libexif/libexif/${versionString}/libexif-${versionString}.tar.bz2"
              }

let json-glib =
      λ(x : { version : List Natural, patch : Natural }) →
          mkGnomeNinja "json-glib" x
        ⫽ { pkgDeps =
            [ prelude.unbounded "glib"
            , prelude.unbounded "libjpeg-turbo"
            , prelude.unbounded "libpng"
            ]
          , pkgBuildDeps = [ prelude.unbounded "gettext" ]
          , installCommand =
              prelude.ninjaInstallWithPkgConfig
                (prelude.mesonMoves [ "json-glib-1.0.pc" ])
          }

let lcms2 =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "lcms2", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/mm2/Little-CMS/archive/lcms${versionString}.tar.gz"
              , pkgSubdir = "Little-CMS-lcms${versionString}"
              }

let libmypaint =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "libmypaint", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/mypaint/libmypaint/releases/download/v${versionString}/libmypaint-${versionString}.tar.xz"
              , pkgDeps = [ prelude.unbounded "json-c" ]
              , pkgBuildDeps =
                [ prelude.unbounded "intltool", prelude.unbounded "gettext" ]
              }

let json-c =
      λ(x : { version : List Natural, dateStr : Text }) →
        let versionString = "${prelude.showVersion x.version}-${x.dateStr}"

        in    prelude.simplePackage { name = "json-c", version = x.version }
            ⫽ { pkgUrl =
                  "https://github.com/json-c/json-c/archive/json-c-${versionString}.tar.gz"
              , pkgSubdir = "json-c-json-c-${versionString}"
              }

let libopenjpeg =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "libopenjpeg", version = v }
            ⫽ prelude.cmakePackage
            ⫽ { pkgUrl =
                  "https://github.com/uclouvain/openjpeg/archive/v${versionString}.tar.gz"
              , pkgSubdir = "openjpeg-${versionString}"
              , pkgDeps = [ prelude.unbounded "zlib" ]
              , pkgBuildDeps =
                [ prelude.unbounded "make", prelude.unbounded "cmake" ]
              , installCommand =
                  λ(cfg : types.BuildVars) →
                      prelude.cmakeInstall cfg
                    # [ prelude.symlink
                          "lib/openjpeg-2.3/OpenJPEGConfig.cmake"
                          "lib/OpenJPEGConfig.cmake"
                      ]
              }

let libevent =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "libevent", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/libevent/libevent/releases/download/release-${versionString}-stable/libevent-${versionString}-stable.tar.gz"
              , pkgSubdir = "libevent-${versionString}-stable"
              , pkgDeps = [ prelude.unbounded "openssl" ]
              }

let memcached =
      λ(v : List Natural) →
          prelude.simplePackage { name = "memcached", version = v }
        ⫽ { pkgUrl =
              "https://memcached.org/files/memcached-${prelude.showVersion
                                                         v}.tar.gz"
          , pkgDeps = [ prelude.unbounded "libevent" ]
          }

let motif =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "motif", version = v }
            ⫽ { pkgUrl =
                  "https://iweb.dl.sourceforge.net/project/motif/Motif%20${versionString}%20Source%20Code/motif-${versionString}.tar.gz"
              , pkgDeps =
                [ prelude.unbounded "libXft"
                , prelude.unbounded "libpng"
                , prelude.unbounded "freetype"
                ]
              }

let libjpeg =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "libjpeg", version = v }
            ⫽ { pkgUrl =
                  "http://www.ijg.org/files/jpegsrc.v${versionString}c.tar.gz"
              , pkgSubdir = "jpeg-${versionString}c"
              }

let feh =
      let cc = prelude.mkCCArg

      let fehMake =
            λ(cfg : types.BuildVars) → { program = prelude.makeExe cfg.buildOS }

      let fehBuild =
            λ(v : List Natural) →
            λ(cfg : types.BuildVars) →
              [ prelude.call
                  (   prelude.defaultCall
                    ⫽ fehMake cfg
                    ⫽ { arguments = [ "feh.1" ], procDir = Some "man" }
                  )
              , prelude.call
                  (   prelude.defaultCall
                    ⫽ fehMake cfg
                    ⫽ { arguments =
                            cc cfg
                          # [ "CFLAGS=${( prelude.mkCFlags cfg
                                        ).value} -DPACKAGE=\\\"feh\\\" -DPREFIX=\\\"${cfg.installDir}\\\" -DVERSION=\\\"${prelude.showVersion
                                                                                                                            v}\\\" ${( prelude.mkLDFlags
                                                                                                                                         cfg.linkDirs
                                                                                                                                         cfg
                                                                                                                                     ).value}"
                            , "feh"
                            ]
                      , procDir = Some "src"
                      }
                  )
              , prelude.call
                  (   prelude.defaultCall
                    ⫽ fehMake cfg
                    ⫽ { arguments = [ "feh.desktop" ]
                      , procDir = Some "share/applications"
                      }
                  )
              ]

      let fehInstall =
            λ(cfg : types.BuildVars) →
                [ prelude.call
                    (   prelude.defaultCall
                      ⫽ { program = prelude.makeExe cfg.buildOS
                        , arguments =
                          [ "CFLAGS=${(prelude.mkCFlags cfg).value}"
                          , "-j${Natural/show cfg.cpus}"
                          , "PREFIX=${cfg.installDir}"
                          , "install"
                          ]
                        }
                    )
                ]
              # prelude.mkLDPathWrapper cfg "feh"

      in  λ(v : List Natural) →
              prelude.simplePackage { name = "feh", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/derf/feh/archive/${prelude.showVersion
                                                           v}.tar.gz"
              , configureCommand = prelude.doNothing
              , buildCommand = fehBuild v
              , installCommand = fehInstall
              , pkgBuildDeps = [ prelude.unbounded "sed" ]
              , pkgDeps =
                [ prelude.unbounded "imlib2"
                , prelude.unbounded "libXt"
                , prelude.unbounded "libXinerama"
                , prelude.unbounded "curl"
                ]
              }

let imlib2 =
      λ(v : List Natural) →
          prelude.simplePackage { name = "imlib2", version = v }
        ⫽ { pkgUrl =
              "https://downloads.sourceforge.net/enlightenment/imlib2-${prelude.showVersion
                                                                          v}.tar.bz2"
          , pkgDeps =
            [ prelude.unbounded "libXext"
            , prelude.unbounded "freetype"
            , prelude.unbounded "libjpeg"
            , prelude.unbounded "libpng"
            , prelude.unbounded "giflib"
            ]
          }

let jemalloc =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "jemalloc", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/jemalloc/jemalloc/releases/download/${versionString}/jemalloc-${versionString}.tar.bz2"
              }

let gperftools =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "gperftools", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/gperftools/gperftools/releases/download/gperftools-${versionString}/gperftools-${versionString}.tar.gz"
              }

let openssh =
      let opensshInstall =
            λ(cfg : types.BuildVars) →
              [ prelude.call
                  (   prelude.defaultCall
                    ⫽ { program = prelude.makeExe cfg.buildOS
                      , arguments =
                        [ "PRIVSEP_PATH=${cfg.installDir}/var"
                        , "install"
                        , "-j${Natural/show cfg.cpus}"
                        ]
                      , environment = Some
                          (   prelude.defaultPath cfg
                            # [ prelude.mkPkgConfigVar cfg.linkDirs
                              , prelude.libPath cfg
                              ]
                          )
                      }
                  )
              ]

      in  λ(v : List Natural) →
              prelude.simplePackage { name = "openssh", version = v }
            ⫽ { pkgUrl =
                  "https://mirrors.gigenet.com/pub/OpenBSD/OpenSSH/portable/openssh-${prelude.showVersion
                                                                                        v}p1.tar.gz"
              , pkgSubdir = "openssh-${prelude.showVersion v}p1"
              , installCommand = opensshInstall
              , pkgDeps = [ prelude.unbounded "openssl" ]
              }

let libxslt =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libxslt", version = v }
        ⫽ { pkgUrl =
              "http://xmlsoft.org/sources/libxslt-${prelude.showVersion
                                                      v}.tar.gz"
          , pkgDeps = [ prelude.unbounded "libxml2" ]
          }

let libepoxy =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.ninjaPackage { name = "libepoxy", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/anholt/libepoxy/releases/download/${versionString}/libepoxy-${versionString}.tar.xz"
              , pkgDeps = [ prelude.unbounded "mesa" ]
              , installCommand =
                  prelude.ninjaInstallWithPkgConfig
                    (prelude.mesonMoves [ "epoxy.pc" ])
              }

let mesa =
      λ(v : List Natural) →
          prelude.simplePackage { name = "mesa", version = v }
        ⫽ { pkgUrl =
              "https://mesa.freedesktop.org/archive/mesa-${prelude.showVersion
                                                             v}.tar.xz"
          , pkgDeps =
            [ prelude.lowerBound { name = "libdrm", lower = [ 2, 4, 75 ] }
            , prelude.unbounded "libXdamage"
            , prelude.unbounded "libXfixes"
            , prelude.unbounded "libXxf86vm"
            , prelude.lowerBound { name = "libxshmfence", lower = [ 1, 1 ] }
            , prelude.lowerBound { name = "glproto", lower = [ 1, 4, 14 ] }
            , prelude.lowerBound { name = "dri2proto", lower = [ 2, 8 ] }
            , prelude.unbounded "libXrandr"
            ]
          , configureCommand =
              prelude.configureWithFlags
                [ "--enable-autotools"
                , "--with-gallium-drivers=nouveau,swrast"
                ]
          }

let libXdamage =
      mkXLibDeps
        { name = "libXdamage"
        , deps =
          [ prelude.unbounded "libXfixes", prelude.unbounded "damageproto" ]
        }

let libXfixes =
      mkXLibDeps
        { name = "libXfixes"
        , deps =
          [ prelude.unbounded "xproto"
          , prelude.unbounded "fixesproto"
          , prelude.unbounded "xextproto"
          , prelude.unbounded "libX11"
          ]
        }

let libXxf86vm =
      mkXLibDeps
        { name = "libXxf86vm"
        , deps =
          [ prelude.unbounded "xproto"
          , prelude.unbounded "libX11"
          , prelude.unbounded "xextproto"
          , prelude.unbounded "libXext"
          , prelude.unbounded "xf86vidmodeproto"
          ]
        }

let libxshmfence =
      mkXLibDeps
        { name = "libxshmfence", deps = [ prelude.unbounded "xproto" ] }

let gnome-doc-utils =
      λ(x : { version : List Natural, patch : Natural }) →
          mkGnomeSimple "gnome-doc-utils" x
        ⫽ { pkgDeps =
            [ prelude.lowerBound { name = "libxslt", lower = [ 1, 1, 8 ] }
            , prelude.lowerBound { name = "libxml2", lower = [ 2, 6, 12 ] }
            ]
          , pkgBuildDeps =
            [ prelude.lowerBound { name = "intltool", lower = [ 0, 35, 0 ] }
            , prelude.unbounded "gettext"
            , prelude.unbounded "python2"
            ]
          }

let itstool =
      λ(v : List Natural) →
          prelude.simplePackage { name = "itstool", version = v }
        ⫽ { pkgUrl =
              "http://files.itstool.org/itstool/itstool-${prelude.showVersion
                                                            v}.tar.bz2"
          , pkgDeps = [ prelude.unbounded "libxml2" ]
          }

let gexiv2 =
      λ(x : { version : List Natural, patch : Natural }) →
          mkGnomeNinja "gexiv2" x
        ⫽ { pkgDeps = [ prelude.unbounded "exiv2", prelude.unbounded "glib" ]
          , installCommand =
              prelude.ninjaInstallWithPkgConfig
                (prelude.mesonMoves [ "gexiv2.pc" ])
          }

let exiv2 =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "exiv2", version = v }
            ⫽ prelude.cmakePackage
            ⫽ { pkgUrl =
                  "http://www.exiv2.org/builds/exiv2-${versionString}-Source.tar.gz"
              , pkgSubdir = "exiv2-${versionString}-Source"
              }

let libtiff =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.ninjaPackage { name = "libtiff", version = v }
            ⫽ { pkgUrl =
                  "https://download.osgeo.org/libtiff/tiff-${versionString}.tar.gz"
              , pkgSubdir = "tiff-${versionString}"
              , configureCommand = prelude.cmakeConfigureNinja
              , pkgBuildDeps =
                [ prelude.unbounded "cmake", prelude.unbounded "ninja" ]
              }

let nspr =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        let bitFlag =
              λ(cfg : types.BuildVars) →
                if    prelude.isX64 (prelude.archCfg cfg)
                then  [ "--enable-64bit" ]
                else  [] : List Text

        in    prelude.simplePackage { name = "nspr", version = v }
            ⫽ { pkgUrl =
                  "https://archive.mozilla.org/pub/nspr/releases/v${versionString}/src/nspr-${versionString}.tar.gz"
              , pkgSubdir = "nspr-${versionString}/nspr"
              , configureCommand =
                  λ(cfg : types.BuildVars) →
                    prelude.configureWithFlags (bitFlag cfg) cfg
              }

let libthai =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libthai", version = v }
        ⫽ { pkgUrl =
              "https://linux.thai.net/pub/thailinux/software/libthai/libthai-${prelude.showVersion
                                                                                 v}.tar.xz"
          , pkgDeps = [ prelude.unbounded "libdatrie" ]
          }

let libdatrie =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libdatrie", version = v }
        ⫽ { pkgUrl =
              "https://linux.thai.net/pub/thailinux/software/libthai/libdatrie-${prelude.showVersion
                                                                                   v}.tar.xz"
          }

let joe =
      λ(v : List Natural) →
          prelude.simplePackage { name = "joe", version = v }
        ⫽ { pkgUrl =
              "https://downloads.sourceforge.net/joe-editor/joe-${prelude.showVersion
                                                                    v}.tar.gz"
          , installCommand = prelude.installWithBinaries [ "bin/joe" ]
          }

let fossil =
      λ(v : List Natural) →
          prelude.simplePackage { name = "fossil", version = v }
        ⫽ { pkgUrl =
              "https://fossil-scm.org/fossil/uv/fossil-src-${prelude.showVersion
                                                               v}.tar.gz"
          , installCommand = prelude.installWithBinaries [ "bin/fossil" ]
          , pkgDeps = [ prelude.unbounded "zlib", prelude.unbounded "openssl" ]
          }

let libcroco =
      λ(x : { version : List Natural, patch : Natural }) →
          mkGnomeSimple "libcroco" x
        ⫽ { pkgDeps =
            [ prelude.lowerBound { name = "glib", lower = [ 2, 0 ] }
            , prelude.lowerBound { name = "libxml2", lower = [ 2, 4, 23 ] }
            ]
          }

let libsoup =
      λ(x : { version : List Natural, patch : Natural }) →
        let libsoupCfgFile =
              ''
              option('gssapi',
                type : 'boolean',
                value : true,
                description : 'Build with GSSAPI support'
              )

              option('krb5_config',
                type : 'string',
                description : 'Where to look for krb5-config, path points to krb5-config installation (defaultly looking in PATH)'
              )

              option('ntlm',
                type : 'boolean',
                value : false,
                description : 'Build with NTLM support'
              )

              option('tls_check',
                type : 'boolean',
                value : true,
                description : 'Enable TLS support through glib-networking. If you are building a package, you can disable this to allow building libsoup anyway (since glib-networking is not actually required at compile time), but you should be sure to add a runtime dependency on it.'
              )

              option('gnome',
                type : 'boolean',
                value : true,
                description : 'Build libsoup with GNOME support'
              )

              option('introspection',
                type : 'boolean',
                value : true,
                description : 'Build GObject Introspection data'
              )

              option('vapi',
                type : 'boolean',
                value : false,
                description : 'Build Vala bindings'
              )

              option('doc',
                type: 'boolean',
                value: false,
                description: 'Enable generating the API reference'
              )

              option('tests',
                type: 'boolean',
                value: true,
                description: 'Enable unit tests compilation'
              )
              ''

        in    mkGnomeNinja "libsoup" x
            ⫽ { pkgDeps =
                [ prelude.unbounded "glib"
                , prelude.unbounded "sqlite"
                , prelude.unbounded "libxml2"
                , prelude.unbounded "libpsl"
                , prelude.unbounded "krb5"
                , prelude.unbounded "gobject-introspection"
                ]
              , pkgBuildDeps = [ prelude.unbounded "vala" ]
              , configureCommand =
                  λ(cfg : types.BuildVars) →
                      [ prelude.writeFile
                          { file = "meson_options.txt"
                          , contents = libsoupCfgFile
                          }
                      ]
                    # prelude.mesonConfigure cfg
              , installCommand =
                  prelude.ninjaInstallWithPkgConfig
                    (prelude.mesonMoves [ "libsoup-2.4.pc" ])
              }

let libpsl =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "libpsl", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/rockdaboot/libpsl/releases/download/libpsl-${versionString}/libpsl-${versionString}.tar.gz"
              }

let krb5 =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "krb5", version = v }
            ⫽ { pkgUrl =
                  "https://kerberos.org/dist/krb5/${versionString}/krb5-${versionString}.tar.gz"
              , pkgSubdir = "krb5-${versionString}/src"
              , pkgBuildDeps = [ prelude.unbounded "bison" ]
              }

let vala =
      λ(x : { version : List Natural, patch : Natural }) →
          mkGnomeSimple "vala" x
        ⫽ { pkgBuildDeps = [ prelude.unbounded "flex" ]
          , pkgDeps =
            [ prelude.lowerBound { name = "glib", lower = [ 2, 40, 0 ] }
            , prelude.lowerBound { name = "graphviz", lower = [ 2, 15 ] }
            ]
          }

let htop =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "htop", version = v }
            ⫽ { pkgUrl =
                  "https://hisham.hm/htop/releases/${versionString}/htop-${versionString}.tar.gz"
              , pkgDeps = [ prelude.unbounded "ncurses" ]
              , pkgBuildDeps = [ prelude.unbounded "python3" ]
              , installCommand = prelude.installWithBinaries [ "bin/htop" ]
              }

let mpfr =
      λ(v : List Natural) →
          prelude.simplePackage { name = "mpfr", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/mpfr/mpfr-${prelude.showVersion
                                                     v}.tar.xz"
          }

let libsodium =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libsodium", version = v }
        ⫽ { pkgUrl =
              "https://download.libsodium.org/libsodium/releases/libsodium-${prelude.showVersion
                                                                               v}.tar.gz"
          }

let libev =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libev", version = v }
        ⫽ { pkgUrl =
              "http://dist.schmorp.de/libev/Attic/libev-${prelude.showVersion
                                                            v}.tar.gz"
          }

let ctags =
      λ(v : List Natural) →
          prelude.simplePackage { name = "ctags", version = v }
        ⫽ { pkgUrl =
              "http://prdownloads.sourceforge.net/ctags/ctags-${prelude.showVersion
                                                                  v}.tar.gz"
          , installCommand = prelude.installWithBinaries [ "bin/ctags" ]
          }

let tcc =
      λ(v : List Natural) →
          prelude.simplePackage { name = "tcc", version = v }
        ⫽ { pkgUrl =
              "http://download.savannah.gnu.org/releases/tinycc/tcc-${prelude.showVersion
                                                                        v}.tar.bz2"
          , pkgBuildDeps =
            [ prelude.unbounded "perl", prelude.unbounded "texinfo" ]
          , installCommand = prelude.installWithBinaries [ "bin/tcc" ]
          }

let texinfo =
      λ(v : List Natural) →
          prelude.simplePackage { name = "texinfo", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/texinfo/texinfo-${prelude.showVersion
                                                           v}.tar.xz"
          , configureCommand =
              prelude.configureWithFlags [ "--disable-dependency-tracking" ]
          }

let node =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "node", version = v }
            ⫽ { pkgUrl =
                  "https://nodejs.org/dist/v${versionString}/node-v${versionString}.tar.gz"
              , pkgSubdir = "node-v${versionString}"
              , installCommand =
                  λ(cfg : types.BuildVars) →
                      prelude.installWithBinaries [ "bin/node", "bin/npm" ] cfg
                    # [ prelude.mkExe
                          "${cfg.installDir}/lib/node_modules/npm/bin/npm-cli.js"
                      ]
              }

let glu =
      λ(v : List Natural) →
          prelude.simplePackage { name = "glu", version = v }
        ⫽ { pkgUrl =
              "https://mesa.freedesktop.org/archive/glu/glu-${prelude.showVersion
                                                                v}.tar.gz"
          , pkgDeps = [ prelude.unbounded "mesa" ]
          }

let compositeproto = mkXProto "compositeproto"

let libXcomposite =
      mkXLibDeps
        { name = "libXcomposite"
        , deps =
          [ prelude.unbounded "libX11"
          , prelude.lowerBound { name = "compositeproto", lower = [ 0, 4 ] }
          , prelude.unbounded "libXfixes"
          ]
        }

let mosh =
      λ(v : List Natural) →
          prelude.simplePackage { name = "mosh", version = v }
        ⫽ { pkgUrl = "https://mosh.org/mosh-${prelude.showVersion v}.tar.gz"
          , pkgBuildDeps = [ prelude.unbounded "protobuf" ]
          , pkgDeps =
            [ prelude.unbounded "zlib"
            , prelude.unbounded "protobuf"
            , prelude.unbounded "openssl"
            ]
          , installCommand = prelude.installWithBinaries [ "bin/mosh" ]
          }

let protobuf =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "protobuf", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/protocolbuffers/protobuf/releases/download/v${versionString}/protobuf-cpp-${versionString}.tar.gz"
              , pkgSubdir = "protobuf-${versionString}"
              }

let libcds =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "libcds", version = v }
            ⫽ prelude.cmakePackage
            ⫽ { pkgUrl =
                  "https://downloads.sourceforge.net/libcds/cds-${prelude.showVersion
                                                                    v}.tar.gz"
              , pkgSubdir = "cds-${versionString}"
              , pkgDeps = [ prelude.unbounded "libboost" ]
              }

let libboost =
    -- TODO: use bootstrap.bat on windows
      let boostConfigure =
            λ(cfg : types.BuildVars) →
              [ prelude.call
                  (prelude.defaultCall ⫽ { program = "./bootstrap.sh" })
              ]

      let boostInstall =
            λ(cfg : types.BuildVars) →
              [ prelude.call
                  (   prelude.defaultCall
                    ⫽ { program = "./b2"
                      , arguments =
                        [ "install"
                        , "--prefix=${cfg.installDir}"
                        , "--without-python"
                        ]
                      }
                  )
              ]

      in  λ(v : List Natural) →
            let versionString = prelude.underscoreVersion v

            in    prelude.simplePackage { name = "libboost", version = v }
                ⫽ { pkgUrl =
                      "https://dl.bintray.com/boostorg/release/${prelude.showVersion
                                                                   v}/source/boost_${versionString}.tar.bz2"
                  , pkgSubdir = "boost_${versionString}"
                  , configureCommand = boostConfigure
                  , buildCommand = prelude.doNothing
                  , installCommand = boostInstall
                  }

let slowBuild =
      λ(cfg : types.BuildVars) →
        [ prelude.call
            { program = "cmake"
            , arguments =
              [ "--build", ".", "--config", "Release", "--", "-j", "2" ]
            , environment = prelude.defaultEnv
            , procDir = Some "build"
            }
        ]

let clang =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "clang", version = v }
            ⫽ prelude.cmakePackage
            ⫽ { pkgUrl =
                  "http://releases.llvm.org/${versionString}/cfe-${versionString}.src.tar.xz"
              , pkgSubdir = "cfe-${versionString}.src"
              , pkgDeps = [ prelude.unbounded "llvm" ]
              , buildCommand = slowBuild
              , installCommand =
                  λ(cfg : types.BuildVars) →
                      prelude.cmakeInstall cfg
                    # [ prelude.symlinkBinary "bin/clang"
                      , prelude.symlinkBinary "bin/clang-format"
                      ]
              }

let llvm =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "llvm", version = v }
            ⫽ prelude.cmakePackage
            ⫽ { pkgUrl =
                  "http://releases.llvm.org/${versionString}/llvm-${versionString}.src.tar.xz"
              , pkgSubdir = "llvm-${versionString}.src"
              , buildCommand = slowBuild
              }

let pari =
      λ(v : List Natural) →
          prelude.simplePackage { name = "pari", version = v }
        ⫽ { pkgUrl =
              "http://pari.math.u-bordeaux.fr/pub/pari/unix/pari-${prelude.showVersion
                                                                     v}.tar.gz"
          , configureCommand =
              prelude.generalConfigure
                prelude.configSome
                "Configure"
                ([] : List Text)
                ([] : List Text)
          }

let pdfgrep =
      λ(v : List Natural) →
          prelude.simplePackage { name = "pdfgrep", version = v }
        ⫽ { pkgUrl =
              "https://pdfgrep.org/download/pdfgrep-${prelude.showVersion
                                                        v}.tar.gz"
          , pkgDeps =
            [ prelude.unbounded "poppler", prelude.unbounded "libgcrypt" ]
          , installCommand =
              λ(cfg : types.BuildVars) →
                  prelude.installWithWrappers [ "pdfgrep" ] cfg
                # prelude.symlinkManpages
                    [ { file = "share/man/man1/pdfgrep.1", section = 1 } ]
          }

let mpc =
      λ(v : List Natural) →
          prelude.simplePackage { name = "mpc", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/mpc/mpc-${prelude.showVersion v}.tar.gz"
          , pkgDeps = [ prelude.unbounded "mpfr" ]
          }

let gcc =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "gcc", version = v }
            ⫽ { pkgUrl =
                  "https://ftp.gnu.org/gnu/gcc/gcc-${versionString}/gcc-${versionString}.tar.xz"
              , configureCommand =
                  λ(cfg : types.BuildVars) →
                      [ prelude.call
                          { program = "contrib/download_prerequisites"
                          , arguments = [] : List Text
                          , environment = None (List types.EnvVar)
                          , procDir = None Text
                          }
                      ]
                    # prelude.configureWithFlags [ "--disable-multilib" ] cfg
              , installCommand =
                  prelude.installWithBinaries
                    [ "bin/gcc"
                    , "bin/g++"
                    , "bin/gcc-ar"
                    , "bin/gcc-nm"
                    , "bin/gfortran"
                    , "bin/gcc-ranlib"
                    ]
              , pkgBuildDeps =
                [ prelude.unbounded "curl"
                , prelude.unbounded "sed"
                , prelude.unbounded "libtool"
                ]
              }

let ruby =
      λ(x : { version : List Natural, patch : Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString ++ "." ++ Natural/show x.patch

        in    prelude.simplePackage
                { name = "ruby", version = prelude.fullVersion x }
            ⫽ { pkgUrl =
                  "https://cache.ruby-lang.org/pub/ruby/${versionString}/ruby-${fullVersion}.tar.gz"
              , installCommand =
                  prelude.installWithBinaries [ "bin/ruby", "bin/gem" ]
              , pkgDeps =
                [ prelude.unbounded "readline", prelude.unbounded "openssl" ]
              }

let poppler =
      λ(v : List Natural) →
        let versionString = "20.09.0"

        in    prelude.simplePackage { name = "poppler", version = v }
            ⫽ prelude.cmakePackage
            ⫽ { pkgUrl =
                  "https://poppler.freedesktop.org/poppler-${versionString}.tar.xz"
              , pkgSubdir = "poppler-${versionString}"
              , pkgDeps =
                [ prelude.unbounded "fontconfig"
                , prelude.unbounded "libopenjpeg"
                , prelude.unbounded "libjpeg-turbo"
                , prelude.unbounded "libjpeg"
                , prelude.unbounded "freetype"
                , prelude.unbounded "zlib"
                , prelude.unbounded "libpng"
                , prelude.unbounded "libiconv"
                , prelude.unbounded "harfbuzz"
                , prelude.unbounded "glib"
                ]
              , installCommand =
                  λ(cfg : types.BuildVars) →
                      prelude.cmakeInstall cfg
                    # prelude.mkLDPathWrappers
                        cfg
                        [ "pdfdetach"
                        , "pdffonts"
                        , "pdfimages"
                        , "pdfinfo"
                        , "pdfseparate"
                        , "pdfsig"
                        , "pdftocairo"
                        , "pdftohtml"
                        , "pdftoppm"
                        , "pdftops"
                        , "pdftotext"
                        , "pdfunite"
                        ]
              }

let tesseract =
      λ(v : List Natural) →
          prelude.simplePackage { name = "tesseract", version = v }
        ⫽ { pkgUrl =
              "https://github.com/tesseract-ocr/tesseract/archive/${prelude.showVersion
                                                                      v}.tar.gz"
          , pkgDeps =
            [ prelude.lowerBound { name = "leptonica", lower = [ 1, 74 ] } ]
          , pkgBuildDeps =
            [ prelude.unbounded "libtool"
            , prelude.unbounded "automake"
            , prelude.unbounded "pkg-config"
            ]
          , configureCommand = prelude.autogenConfigure
          , installCommand = prelude.installWithBinaries [ "bin/tesseract" ]
          }

let leptonica =
      λ(v : List Natural) →
          prelude.simplePackage { name = "leptonica", version = v }
        ⫽ { pkgUrl =
              "http://leptonica.org/source/leptonica-${prelude.showVersion
                                                         v}.tar.gz"
          , pkgDeps = [ prelude.unbounded "zlib" ]
          }

let grep =
      λ(v : List Natural) → prelude.makeGnuExe { name = "grep", version = v }

let phash =
      λ(v : List Natural) →
          prelude.simplePackage { name = "pHash", version = v }
        ⫽ { pkgUrl =
              "http://phash.org/releases/pHash-${prelude.showVersion v}.tar.gz"
          , pkgDeps =
            [ prelude.lowerBound { name = "CImg", lower = [ 1, 3 ] }
            , prelude.unbounded "ffmpeg"
            , prelude.unbounded "libsndfile"
            , prelude.unbounded "libsamplerate"
            , prelude.unbounded "mpg123"
            , prelude.unbounded "libjpeg-turbo"
            , prelude.unbounded "libpng"
            , prelude.unbounded "fftw"
            ]
          , pkgBuildDeps =
            [ prelude.unbounded "autoconf"
            , prelude.unbounded "automake"
            , prelude.unbounded "grep"
            , prelude.unbounded "coreutils"
            , prelude.unbounded "sed"
            , prelude.unbounded "libtool"
            ]
          , configureCommand =
              λ(cfg : types.BuildVars) →
                  [ prelude.patch
                      "./pHash.patch"
                  , prelude.call
                      { program = "autoreconf"
                      , arguments = [ "-i" ]
                      , environment = Some
                        [ { var = "PATH"
                          , value = prelude.mkPathVar cfg.binDirs
                          }
                        , prelude.mkAclocalPath cfg.shareDirs
                        ]
                      , procDir = None Text
                      }
                  ]
                # prelude.defaultConfigure cfg
          }

let cimg =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "CImg", version = v }
            ⫽ { pkgUrl = "http://cimg.eu/files/CImg_${versionString}.zip"
              , pkgBuildDeps = [] : List types.Dep
              , configureCommand = prelude.doNothing
              , buildCommand = prelude.doNothing
              , installCommand =
                  λ(_ : types.BuildVars) →
                    [ prelude.copyFile "CImg.h" "include/CImg.h" ]
              }

let ffmpeg =
      λ(v : List Natural) →
          prelude.simplePackage { name = "ffmpeg", version = v }
        ⫽ { pkgUrl =
              "https://ffmpeg.org/releases/ffmpeg-${prelude.showVersion
                                                      v}.tar.xz"
          , pkgBuildDeps = [ prelude.unbounded "nasm" ]
          , configureCommand =
              prelude.configureWithFlags
                [ "--enable-shared"
                , "--enable-libmp3lame"
                , "--enable-gpl"
                , "--enable-version3"
                , "--enable-nonfree"
                , "--disable-debug"
                , "--enable-libass"
                , "--enable-libfreetype"
                , "--enable-libvorbis"
                , "--enable-avresample"
                , "--enable-gnutls"
                , "--enable-libvpx"
                , "--enable-libfdk-aac"
                ]
          , installCommand = prelude.installWithWrappers [ "ffmpeg" ]
          , pkgDeps =
            [ prelude.unbounded "bzip2"
            , prelude.unbounded "libmp3lame"
            , prelude.unbounded "libass"
            , prelude.unbounded "freetype"
            , prelude.unbounded "libvorbis"
            , prelude.unbounded "gnutls"
            , prelude.unbounded "libvpx"
            , prelude.unbounded "fdk-aac"
            ]
          }

let libsndfile =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libsndfile", version = v }
        ⫽ { pkgUrl =
              "http://www.mega-nerd.com/libsndfile/files/libsndfile-${prelude.showVersion
                                                                        v}.tar.gz"
          }

let libsamplerate =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libsamplerate", version = v }
        ⫽ { pkgUrl =
              "http://www.mega-nerd.com/SRC/libsamplerate-${prelude.showVersion
                                                              v}.tar.gz"
          }

let mpg123 =
      λ(v : List Natural) →
          prelude.simplePackage { name = "mpg123", version = v }
        ⫽ { pkgUrl =
              "http://www.mpg123.de/download/mpg123-${prelude.showVersion
                                                        v}.tar.bz2"
          }

let time =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "time", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/time/time-${prelude.showVersion
                                                     v}.tar.gz"
          }

let make =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "make", version = v }
        ⫽ { pkgUrl =
              "https://ftp.wayne.edu/gnu/make/make-${prelude.showVersion
                                                       v}.tar.lz"
          , buildCommand =
              λ(cfg : types.BuildVars) →
                [ prelude.call
                    (   prelude.defaultCall
                      ⫽ { program = "sh"
                        , arguments = [ "build.sh" ]
                        , environment = Some (prelude.buildEnv cfg)
                        }
                    )
                ]
          , installCommand =
              λ(_ : types.BuildVars) →
                [ prelude.copyFile "make" "bin/make"
                , prelude.symlinkBinary "bin/make"
                ]
          , pkgBuildDeps = [ prelude.unbounded "patch" ]
          }

let mercury =
      let mercuryBuild =
            λ(cfg : types.BuildVars) →
              [ prelude.call
                  (   prelude.defaultCall
                    ⫽ { program = "make"
                      , arguments = [ "PARALLEL=-j${Natural/show cfg.cpus}" ]
                      , environment = Some (prelude.buildEnv cfg)
                      }
                  )
              ]

      let mercuryCommon =
            { pkgUrl =
                "http://dl.mercurylang.org/release/mercury-srcdist-14.01.1.tar.gz"
            , pkgSubdir = "mercury-srcdist-14.01.1"
            , buildCommand = mercuryBuild
            }

      in    prelude.simplePackage { name = "mercury", version = [ 14, 1, 1 ] }
          ⫽ mercuryCommon
          ⫽ { pkgBuildDeps = [ prelude.unbounded "flex" ] }

let qt =
      λ(x : { version : List Natural, patch : Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString ++ "." ++ Natural/show x.patch

        in    prelude.simplePackage
                { name = "qt", version = prelude.fullVersion x }
            ⫽ { pkgUrl =
                  "https://download.qt.io/archive/qt/${versionString}/${fullVersion}/single/qt-everywhere-src-${fullVersion}.tar.xz"
              , pkgSubdir = "qt-everywhere-src-${fullVersion}"
              , pkgBuildDeps =
                [ prelude.unbounded "flex"
                , prelude.unbounded "bison"
                , prelude.unbounded "pkg-config"
                , prelude.unbounded "gperf"
                , prelude.unbounded "perl"
                , prelude.unbounded "python2"
                , prelude.unbounded "git"
                ]
              , pkgDeps =
                [ prelude.unbounded "fontconfig"
                , prelude.unbounded "mesa"
                , prelude.unbounded "dbus"
                , prelude.unbounded "freetype"
                , prelude.unbounded "harfbuzz"
                , prelude.unbounded "libjpeg-turbo"
                , prelude.unbounded "libpng"
                , prelude.unbounded "giflib"
                , prelude.unbounded "glib"
                ]
              }

let lz4 =
      λ(v : List Natural) →
          prelude.simplePackage { name = "lz4", version = v }
        ⫽ { pkgUrl =
              "https://github.com/lz4/lz4/archive/v${prelude.showVersion
                                                       v}.tar.gz"
          , configureCommand = prelude.doNothing
          , installCommand =
              λ(cfg : types.BuildVars) →
                  [ prelude.call
                      (   prelude.defaultCall
                        ⫽ { program = "make"
                          , arguments =
                            [ "PREFIX=${cfg.installDir}", "install" ]
                          , environment = Some (prelude.buildEnv cfg)
                          }
                      )
                  , prelude.symlinkBinary "bin/lz4"
                  ]
                # prelude.symlinkManpages
                    [ { file = "share/man/man1/lz4.1", section = 1 } ]
          }

let fftw =
      λ(v : List Natural) →
          prelude.simplePackage { name = "fftw", version = v }
        ⫽ { pkgUrl = "http://www.fftw.org/fftw-${prelude.showVersion v}.tar.gz"
          , configureCommand =
              prelude.configureWithFlags
                [ "--enable-shared"
                , "--enable-threads"
                , "--with-combined-threads"
                ]
          }

let icu-le-hb =
      λ(v : List Natural) →
          prelude.simplePackage { name = "icu-le-hb", version = v }
        ⫽ { pkgUrl =
              "https://github.com/harfbuzz/icu-le-hb/archive/${prelude.showVersion
                                                                 v}.tar.gz"
          , configureCommand = prelude.autogenConfigure
          , pkgDeps = [ prelude.unbounded "harfbuzz", prelude.unbounded "icu" ]
          , pkgBuildDeps = [ prelude.unbounded "pkg-config" ]
          }

let icu =
      λ(v : List Natural) →
          prelude.simplePackage { name = "icu", version = v }
        ⫽ { pkgUrl =
              "http://download.icu-project.org/files/icu4c/${prelude.showVersion
                                                               v}/icu4c-${prelude.underscoreVersion
                                                                            v}-src.tgz"
          , pkgSubdir = "icu/source"
          , pkgBuildDeps =
            [ prelude.lowerBound { name = "make", lower = [ 3, 80 ] }
            , prelude.unbounded "python3"
            ]
          }

let opencv =
      λ(v : List Natural) →
          prelude.simplePackage { name = "opencv", version = v }
        ⫽ prelude.cmakePackage
        ⫽ { pkgUrl =
              "https://github.com/opencv/opencv/archive/${prelude.showVersion
                                                            v}.zip"
          , pkgBuildDeps =
            [ prelude.lowerBound { name = "cmake", lower = [ 2, 8, 7 ] }
            , prelude.unbounded "pkg-config"
            , prelude.unbounded "python2"
            ]
          , pkgDeps =
            [ prelude.unbounded "zlib"
            , prelude.unbounded "libjpeg-turbo"
            , prelude.unbounded "libpng"
            , prelude.unbounded "gtk3"
            , prelude.unbounded "ffmpeg"
            ]
          }

let libraw =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libraw", version = v }
        ⫽ { pkgUrl =
              "https://www.libraw.org/data/LibRaw-${prelude.showVersion
                                                      v}.tar.gz"
          , pkgSubdir = "LibRaw-${prelude.showVersion v}"
          }

let quazip =
      λ(v : List Natural) →
          prelude.simplePackage { name = "quazip", version = v }
        ⫽ prelude.cmakePackage
        ⫽ { pkgUrl =
              "https://github.com/stachenov/quazip/archive/v${prelude.showVersion
                                                                v}.tar.gz"
          , pkgDeps = [ prelude.unbounded "zlib", prelude.unbounded "qt" ]
          }

let eigen =
      λ(v : List Natural) →
          prelude.simplePackage { name = "eigen", version = v }
        ⫽ prelude.cmakePackage
        ⫽ { pkgUrl =
              "http://bitbucket.org/eigen/eigen/get/${prelude.showVersion
                                                        v}.tar.bz2"
          , pkgSubdir = "eigen-eigen-323c052e1731"
          }

let blas =
      λ(v : List Natural) →
          prelude.simplePackage { name = "blas", version = v }
        ⫽ { pkgUrl =
              "http://www.netlib.org/blas/blas-${prelude.showVersion v}.tgz"
          , pkgSubdir = "BLAS-${prelude.showVersion v}"
          , pkgBuildDeps = [ prelude.unbounded "gcc" ]
          , configureCommand = prelude.doNothing
          , installCommand =
              λ(_ : types.BuildVars) →
                [ prelude.copyFile "blas_LINUX.a" "lib/blas.a" ]
          }

let openblas =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "openblas", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/xianyi/OpenBLAS/archive/v${versionString}.tar.gz"
              , pkgSubdir = "OpenBLAS-${versionString}"
              , pkgBuildDeps = [ prelude.unbounded "gcc" ]
              , pkgDeps = [ prelude.unbounded "gcc" ]
              , configureCommand = prelude.doNothing
              , installCommand = prelude.installPrefix
              }

let r =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "r", version = v }
            ⫽ { pkgUrl =
                  "https://repo.miserver.it.umich.edu/cran/src/base/R-4/R-${versionString}.tar.gz"
              , pkgSubdir = "R-${versionString}"
              , pkgDeps =
                [ prelude.unbounded "readline"
                , prelude.unbounded "libXt"
                , prelude.unbounded "pcre"
                ]
              , pkgBuildDeps = [ prelude.unbounded "gcc" ]
              , installCommand =
                  prelude.installWithBinaries [ "bin/R", "bin/Rscript" ]
              }

let libspng =
      λ(v : List Natural) →
          prelude.ninjaPackage { name = "libspng", version = v }
        ⫽ { pkgUrl =
              "https://github.com/randy408/libspng/archive/v${prelude.showVersion
                                                                v}.tar.gz"
          , pkgBuildDeps =
            [ prelude.unbounded "pkg-config"
            , prelude.unbounded "meson"
            , prelude.lowerBound { name = "ninja", lower = [ 1, 5, 0 ] }
            ]
          , pkgDeps = [ prelude.unbounded "zlib" ]
          }

let glib-networking =
      λ(x : { version : List Natural, patch : Natural }) →
        let versionString = prelude.showVersion x.version

        let fullVersion = versionString ++ "." ++ Natural/show x.patch

        in    prelude.ninjaPackage
                { name = "glib-networking", version = prelude.fullVersion x }
            ⫽ { pkgUrl =
                  "http://ftp.gnome.org/pub/gnome/sources/glib-networking/${versionString}/glib-networking-${fullVersion}.tar.xz"
              , pkgBuildDeps =
                [ prelude.unbounded "pkg-config", prelude.unbounded "gettext" ]
              , pkgDeps =
                [ prelude.unbounded "glib", prelude.unbounded "gnutls" ]
              }

let libwebp =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libwebp", version = v }
        ⫽ { pkgUrl =
              "https://storage.googleapis.com/downloads.webmproject.org/releases/webp/libwebp-${prelude.showVersion
                                                                                                  v}.tar.gz"
          }

let rustc =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "rustc", version = v }
            ⫽ { pkgUrl =
                  "https://static.rust-lang.org/dist/rust-${versionString}-x86_64-unknown-linux-gnu.tar.gz"
              , pkgSubdir = "rust-${versionString}-x86_64-unknown-linux-gnu"
              , configureCommand = prelude.doNothing
              , buildCommand = prelude.doNothing
              , installCommand =
                  λ(cfg : types.BuildVars) →
                    [ prelude.call
                        (   prelude.defaultCall
                          ⫽ { program = "./install.sh"
                            , arguments =
                              [ "--prefix=${cfg.installDir}"
                              , "--disable-ldconfig"
                              ]
                            }
                        )
                    ]
              }

let librsvg =
      λ(x : { version : List Natural, patch : Natural }) →
          mkGnomeSimple "librsvg" x
        ⫽ { pkgBuildDeps =
            [ prelude.lowerBound { name = "rustc", lower = [ 1, 16, 0 ] }
            , prelude.unbounded "pkg-config"
            ]
          , pkgDeps =
            [ prelude.lowerBound { name = "cairo", lower = [ 1, 16, 0 ] }
            , prelude.lowerBound { name = "libxml2", lower = [ 2, 9, 0 ] }
            , prelude.lowerBound { name = "libcroco", lower = [ 0, 6, 1 ] }
            , prelude.unbounded "pango"
            , prelude.lowerBound { name = "gdk-pixbuf", lower = [ 2, 20 ] }
            , prelude.unbounded "gobject-introspection"
            , prelude.lowerBound { name = "glib", lower = [ 2, 10, 0 ] }
            ]
          }

let ats =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        let atsBuild =
              λ(cfg : types.BuildVars) →
                let buildDir =
                      cfg.currentDir ++ "/ATS2-Postiats-gmp-${versionString}"

                in  [ prelude.call
                        (   prelude.defaultCall
                          ⫽ { program = prelude.makeExe cfg.buildOS
                            , arguments =
                              [ "CFLAGS=${( prelude.mkCFlags cfg
                                          ).value} -I${buildDir}/src/CBOOT/ccomp/runtime -I${buildDir}/src/CBOOT"
                              , "LDFLAGS='${( prelude.mkLDFlags cfg.linkDirs cfg
                                            ).value}'"
                              ]
                            , environment = Some (prelude.buildEnv cfg)
                            }
                        )
                    ]

        in    prelude.simplePackage { name = "ats", version = v }
            ⫽ { pkgUrl =
                  "http://ats-lang.sourceforge.net/IMPLEMENT/Postiats/ATS2-Postiats-gmp-${versionString}.tgz"
              , pkgSubdir = "ATS2-Postiats-gmp-${versionString}"
              , pkgDeps = [ prelude.unbounded "gmp" ]
              , buildCommand = atsBuild
              , installCommand =
                  prelude.installWithBinaries [ "bin/patsopt", "bin/patscc" ]
              }

let libiconv =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libiconv", version = v }
        ⫽ { pkgUrl =
              "https://ftp.wayne.edu/gnu/libiconv/libiconv-${prelude.showVersion
                                                               v}.tar.gz"
          }

let libav =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libav", version = v }
        ⫽ { pkgUrl =
              "https://libav.org/releases/libav-${prelude.showVersion v}.tar.xz"
          , pkgBuildDeps =
            [ prelude.unbounded "nasm", prelude.unbounded "perl" ]
          , installCommand =
              prelude.installWithBinaries [ "bin/avconv", "bin/avprobe" ]
          }

let alsa-lib =
      λ(v : List Natural) →
          prelude.simplePackage { name = "alsa-lib", version = v }
        ⫽ { pkgUrl =
              "https://www.alsa-project.org/files/pub/lib/alsa-lib-${prelude.showVersion
                                                                       v}.tar.bz2"
          }

let bash-completion =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "bash-completion", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/scop/bash-completion/releases/download/${versionString}/bash-completion-${versionString}.tar.xz"
              }

let hugs =
      let hugsEnv =
            λ(_ : List Text) →
            λ(cfg : types.BuildVars) →
              Some
                [ { var = "CFLAGS", value = "-std=gnu89" }
                , { var = "PATH", value = prelude.mkPathVar cfg.binDirs }
                ]

      in    prelude.simplePackage { name = "hugs", version = [ 2006, 9 ] }
          ⫽ { pkgUrl =
                "https://www.haskell.org/hugs/downloads/2006-09/hugs98-plus-Sep2006.tar.gz"
            , pkgSubdir = "hugs98-plus-Sep2006"
            , configureCommand =
                prelude.generalConfigure
                  hugsEnv
                  "configure"
                  ([] : List Text)
                  ([] : List Text)
            , pkgBuildDeps =
              [ prelude.unbounded "coreutils"
              , prelude.unbounded "sed"
              , prelude.unbounded "gcc"
              , prelude.unbounded "binutils"
              , prelude.unbounded "grep"
              , prelude.unbounded "findutils"
              ]
            }

let bash =
      λ(v : List Natural) →
          prelude.simplePackage { name = "bash", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/bash/bash-${prelude.showVersion
                                                     v}.tar.gz"
          }

let findutils =
      λ(v : List Natural) →
          prelude.simplePackage { name = "findutils", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/pub/gnu/findutils/findutils-${prelude.showVersion
                                                                   v}.tar.xz"
          }

let ghc =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "ghc", version = v }
            ⫽ { pkgUrl =
                  "https://downloads.haskell.org/~ghc/${versionString}/ghc-${versionString}-x86_64-deb10-linux.tar.xz"
              , buildCommand = prelude.doNothing
              }

let cmark =
      λ(v : List Natural) →
          prelude.simplePackage { name = "cmark", version = v }
        ⫽ prelude.cmakePackage
        ⫽ { pkgUrl =
              "https://github.com/commonmark/cmark/archive/${prelude.showVersion
                                                               v}.tar.gz"
          }

let lzip =
      λ(v : List Natural) →
          prelude.simplePackage { name = "lzip", version = v }
        ⫽ { pkgUrl =
              "http://download.savannah.gnu.org/releases/lzip/lzip-${prelude.showVersion
                                                                       v}.tar.lz"
          , installCommand =
              λ(cfg : types.BuildVars) →
                  prelude.installWithBinaries [ "bin/lzip" ] cfg
                # prelude.symlinkManpages
                    [ { file = "share/man/man1/lzip.1", section = 1 } ]
          }

let clzip =
      λ(v : List Natural) →
          prelude.simplePackage { name = "clzip", version = v }
        ⫽ { pkgUrl =
              "http://download.savannah.gnu.org/releases/lzip/clzip/clzip-${prelude.showVersion
                                                                              v}.tar.lz"
          , installCommand =
              λ(cfg : types.BuildVars) →
                  prelude.installWithBinaries [ "bin/clzip" ] cfg
                # prelude.symlinkManpages
                    [ { file = "share/man/man1/clzip.1", section = 1 } ]
          }

let lunzip =
      λ(v : List Natural) →
          prelude.simplePackage { name = "lunzip", version = v }
        ⫽ { pkgUrl =
              "http://download.savannah.gnu.org/releases/lzip/lunzip/lunzip-${prelude.showVersion
                                                                                v}.tar.lz"
          , installCommand =
              λ(cfg : types.BuildVars) →
                  prelude.installWithBinaries [ "bin/lunzip" ] cfg
                # prelude.symlinkManpages
                    [ { file = "share/man/man1/lunzip.1", section = 1 } ]
          }

let lzlib =
      λ(v : List Natural) →
          prelude.simplePackage { name = "lzlib", version = v }
        ⫽ { pkgUrl =
              "http://download.savannah.gnu.org/releases/lzip/lzlib/lzlib-${prelude.showVersion
                                                                              v}.tar.lz"
          , configureCommand = prelude.configureWithFlags [ "--enable-shared" ]
          }

let lziprecover =
      λ(v : List Natural) →
          prelude.simplePackage { name = "lziprecover", version = v }
        ⫽ { pkgUrl =
              "http://download.savannah.gnu.org/releases/lzip/lziprecover/lziprecover-${prelude.showVersion
                                                                                          v}.tar.lz"
          , installCommand = prelude.installWithBinaries [ "bin/lziprecover" ]
          }

let libmp3lame =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "libmp3lame", version = v }
            ⫽ { pkgUrl =
                  "https://downloads.sourceforge.net/lame/lame-${versionString}.tar.gz"
              , pkgSubdir = "lame-${versionString}"
              }

let libass =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "libass", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/libass/libass/releases/download/${versionString}/libass-${versionString}.tar.xz"
              , pkgBuildDeps = [ prelude.unbounded "nasm" ]
              , pkgDeps =
                [ prelude.lowerBound { name = "freetype", lower = [ 9, 10, 3 ] }
                , prelude.lowerBound { name = "fribidi", lower = [ 0, 19, 0 ] }
                , prelude.lowerBound { name = "harfbuzz", lower = [ 0, 9, 5 ] }
                , prelude.lowerBound
                    { name = "fontconfig", lower = [ 2, 10, 92 ] }
                ]
              }

let libogg =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libogg", version = v }
        ⫽ { pkgUrl =
              "https://downloads.xiph.org/releases/ogg/libogg-${prelude.showVersion
                                                                  v}.tar.xz"
          }

let libvorbis =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libvorbis", version = v }
        ⫽ { pkgUrl =
              "https://downloads.xiph.org/releases/vorbis/libvorbis-${prelude.showVersion
                                                                        v}.tar.xz"
          , pkgDeps = [ prelude.unbounded "libogg" ]
          }

let libvpx =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "libvpx", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/webmproject/libvpx/archive/v${versionString}/libvpx-${versionString}.tar.gz"
              , pkgBuildDeps =
                [ prelude.unbounded "nasm", prelude.unbounded "perl" ]
              }

let fdk-aac =
      λ(v : List Natural) →
          prelude.simplePackage { name = "fdk-aac", version = v }
        ⫽ { pkgUrl =
              "https://downloads.sourceforge.net/opencore-amr/fdk-aac-${prelude.showVersion
                                                                          v}.tar.gz"
          }

let swi-prolog =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.ninjaPackage { name = "swi-prolog", version = v }
            ⫽ { pkgUrl =
                  "https://www.swi-prolog.org/download/stable/src/swipl-${versionString}.tar.gz"
              , configureCommand = prelude.cmakeConfigureNinja
              , pkgBuildDeps =
                [ prelude.unbounded "cmake"
                , prelude.unbounded "ninja"
                , prelude.unbounded "coreutils"
                ]
              , pkgSubdir = "swipl-${versionString}"
              }

let exiftool =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "exiftool", version = v }
            ⫽ { pkgUrl =
                  "https://sno.phy.queensu.ca/~phil/exiftool/Image-ExifTool-${versionString}.tar.gz"
              , pkgSubdir = "Image-ExifTool-${versionString}"
              , configureCommand = prelude.perlConfigure
              , installCommand =
                  λ(cfg : types.BuildVars) →
                    let perlWrapper =
                          "PERL5LIB=${cfg.installDir}/lib/site_perl/5.30.0/ ${cfg.installDir}/bin/exiftool \$@"

                    let wrapped = "wrapper/exiftool"

                    in    prelude.defaultInstall cfg
                        # [ prelude.createDir "wrapper"
                          , prelude.writeFile
                              { file = wrapped, contents = perlWrapper }
                          , prelude.mkExe wrapped
                          , prelude.copyFile wrapped wrapped
                          , prelude.symlinkBinary wrapped
                          ]
              , pkgBuildDeps = [ prelude.unbounded "perl" ]
              }

let subversion =
      λ(v : List Natural) →
          prelude.simplePackage { name = "subversion", version = v }
        ⫽ { pkgUrl =
              "https://www-eu.apache.org/dist/subversion/subversion-${prelude.showVersion
                                                                        v}.tar.bz2"
          , pkgDeps =
            [ prelude.unbounded "apr"
            , prelude.unbounded "apr-util"
            , prelude.unbounded "sqlite"
            , prelude.unbounded "lz4"
            , prelude.unbounded "zlib"
            , prelude.unbounded "utf8proc"
            ]
          , pkgBuildDeps = [ prelude.unbounded "pkg-config" ]
          , installCommand = prelude.installWithBinaries [ "bin/svn" ]
          }

let utf8proc =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "utf8proc", version = v }
            ⫽ prelude.cmakePackage
            ⫽ { pkgUrl =
                  "https://github.com/JuliaStrings/utf8proc/archive/v${versionString}.tar.gz"
              }

let apr =
      λ(v : List Natural) →
          prelude.simplePackage { name = "apr", version = v }
        ⫽ { pkgUrl =
              "https://www-eu.apache.org/dist/apr/apr-${prelude.showVersion
                                                          v}.tar.bz2"
          }

let apr-util =
      λ(v : List Natural) →
          prelude.simplePackage { name = "apr-util", version = v }
        ⫽ { pkgUrl =
              "https://www-eu.apache.org/dist/apr/apr-util-${prelude.showVersion
                                                               v}.tar.bz2"
          , pkgDeps = [ prelude.unbounded "apr" ]
          , configureCommand =
              λ(cfg : types.BuildVars) →
                prelude.configureWithFlags
                  [ "--with-apr=${concat cfg.linkDirs}/../" ]
                  cfg
          }

let libsass =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libsass", version = v }
        ⫽ { pkgUrl =
              "https://github.com/sass/libsass/archive/${prelude.showVersion
                                                           v}.tar.gz"
          , pkgBuildDeps =
            [ prelude.unbounded "autoconf"
            , prelude.unbounded "automake"
            , prelude.unbounded "m4"
            , prelude.unbounded "grep"
            , prelude.unbounded "sed"
            , prelude.unbounded "coreutils"
            , prelude.unbounded "libtool"
            ]
          , configureCommand =
              λ(cfg : types.BuildVars) →
                  [ prelude.call
                      { program = "autoreconf"
                      , arguments = [ "-i" ]
                      , environment = Some
                        [ { var = "PATH"
                          , value = prelude.mkPathVar cfg.binDirs
                          }
                        , prelude.mkAclocalPath cfg.shareDirs
                        ]
                      , procDir = None Text
                      }
                  ]
                # prelude.defaultConfigure cfg
          }

let zstd =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "zstd", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/facebook/zstd/releases/download/v${versionString}/zstd-${versionString}.tar.zst"
              , configureCommand = prelude.doNothing
              , installCommand =
                  λ(cfg : types.BuildVars) →
                      [ prelude.call
                          { program = "make"
                          , arguments =
                            [ "PREFIX=${cfg.installDir}", "install" ]
                          , environment = Some (prelude.buildEnv cfg)
                          , procDir = None Text
                          }
                      , prelude.symlinkBinary "bin/zstd"
                      ]
                    # prelude.symlinkManpages
                        [ { file = "share/man/man1/zstd.1", section = 1 } ]
              }

let luarocks =
      λ(v : List Natural) →
          prelude.simplePackage { name = "luarocks", version = v }
        ⫽ { pkgUrl =
              "http://luarocks.org/releases/luarocks-${prelude.showVersion
                                                         v}.tar.gz"
          , pkgBuildDeps = [ prelude.unbounded "lua" ]
          , installCommand = prelude.installWithWrappers [ "luarocks" ]
          }

let star =
      λ(v : List Natural) →
          prelude.simplePackage { name = "star", version = v }
        ⫽ { pkgUrl =
              "https://downloads.sourceforge.net/s-tar/star-${prelude.showVersion
                                                                v}.tar.bz2"
          , configureCommand = prelude.doNothing
          , installCommand =
              λ(cfg : types.BuildVars) →
                  [ prelude.call
                      (   prelude.defaultCall
                        ⫽ { program = "make"
                          , arguments =
                            [ "DESTDIR=${cfg.installDir}", "install" ]
                          , environment = Some (prelude.buildEnv cfg)
                          }
                      )
                  , prelude.symlinkBinary "opt/schily/bin/star"
                  , prelude.symlinkBinary "opt/schily/bin/spax"
                  ]
                # prelude.symlinkManpages
                    [ { file = "opt/schily/share/man/man1/star.1", section = 1 }
                    , { file = "opt/schily/share/man/man1/scpio.1"
                      , section = 1
                      }
                    , { file = "opt/schily/share/man/man1/spax.1", section = 1 }
                    ]
          }

let smake =
      λ(v : List Natural) →
          prelude.simplePackage { name = "smake", version = v }
        ⫽ { pkgUrl =
              "https://downloads.sourceforge.net/s-make/smake-${prelude.showVersion
                                                                  v}.tar.bz2"
          , configureCommand = prelude.doNothing
          , installCommand =
              λ(cfg : types.BuildVars) →
                  [ prelude.call
                      (   prelude.defaultCall
                        ⫽ { program = "make"
                          , arguments =
                            [ "DESTDIR=${cfg.installDir}", "install" ]
                          , environment = Some (prelude.buildEnv cfg)
                          }
                      )
                  , prelude.symlinkBinary "opt/schily/bin/smake"
                  ]
                # prelude.symlinkManpages
                    [ { file = "opt/schily/share/man/man1/smake.1"
                      , section = 1
                      }
                    , { file = "opt/schily/share/man/man5/makefiles.5"
                      , section = 5
                      }
                    , { file = "opt/schily/share/man/man5/makerules.5"
                      , section = 5
                      }
                    ]
          }

let busybox =
      λ(v : List Natural) →
          prelude.simplePackage { name = "busybox", version = v }
        ⫽ { pkgUrl =
              "https://www.busybox.net/downloads/busybox-${prelude.showVersion
                                                             v}.tar.bz2"
          , configureCommand =
              λ(cfg : types.BuildVars) →
                [ prelude.call
                    (   prelude.defaultCall
                      ⫽ { program = "make"
                        , arguments = [ "defconfig" ]
                        , environment = Some (prelude.buildEnv cfg)
                        }
                    )
                ]
          , installCommand =
              λ(cfg : types.BuildVars) →
                [ prelude.call
                    (   prelude.defaultCall
                      ⫽ { program = "make"
                        , arguments =
                          [ "CONFIG_PREFIX=${cfg.installDir}", "install" ]
                        , environment = Some (prelude.buildEnv cfg)
                        }
                    )
                , prelude.symlinkBinary "bin/busybox"
                ]
          }

let tarlz =
      λ(v : List Natural) →
          prelude.simplePackage { name = "tarlz", version = v }
        ⫽ { pkgUrl =
              "http://download.savannah.gnu.org/releases/lzip/tarlz/tarlz-${prelude.showVersion
                                                                              v}.tar.lz"
          , installCommand =
              λ(cfg : types.BuildVars) →
                  prelude.installWithBinaries [ "bin/tarlz" ] cfg
                # prelude.symlinkManpages
                    [ { file = "share/man/man1/tarlz.1", section = 1 } ]
          }

let lzo =
      λ(v : List Natural) →
          prelude.simplePackage { name = "lzo", version = v }
        ⫽ { pkgUrl =
              "http://www.oberhumer.com/opensource/lzo/download/lzo-${prelude.showVersion
                                                                        v}.tar.gz"
          , pkgBuildDeps = [ prelude.unbounded "binutils" ]
          }

let rlwrap =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "rlwrap", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/hanslub42/rlwrap/releases/download/${versionString}/rlwrap-${versionString}.tar.gz"
              }

let guile =
      λ(v : List Natural) →
          prelude.simplePackage { name = "guile", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/guile/guile-${prelude.showVersion
                                                       v}.tar.gz"
          , pkgDeps = [ prelude.unbounded "unistring", prelude.unbounded "gc" ]
          }

let libgit2 =
      λ(v : List Natural) →
          prelude.simplePackage { name = "libgit2", version = v }
        ⫽ prelude.cmakePackage
        ⫽ { pkgUrl =
              let versionString = prelude.showVersion v

              in  "https://github.com/libgit2/libgit2/releases/download/v${versionString}/libgit2-${versionString}.tar.gz"
          }

let bytestructures =
      λ(v : List Natural) →
          prelude.simplePackage { name = "bytestructures", version = v }
        ⫽ { pkgUrl =
              let versionString = prelude.showVersion v

              in  "https://github.com/TaylanUB/scheme-bytestructures/releases/download/v${versionString}/bytestructures-${versionString}.tar.gz"
          , pkgDeps = [ prelude.unbounded "guile" ]
          }

let guile-git =
      λ(v : List Natural) →
          prelude.simplePackage { name = "guile-git", version = v }
        ⫽ { pkgUrl =
              let versionString = prelude.showVersion v

              in  "https://gitlab.com/guile-git/guile-git/uploads/4c563d8e7e1ff84396abe8ca7011bcaf/guile-git-${versionString}.tar.gz"
          , pkgDeps =
            [ prelude.unbounded "guile"
            , prelude.unbounded "bytestructures"
            , prelude.unbounded "libgit2"
            ]
          }

let guile-json =
      λ(v : List Natural) →
          prelude.simplePackage { name = "guile-json", version = v }
        ⫽ { pkgUrl =
              "http://download.savannah.gnu.org/releases/guile-json/guile-json-${prelude.showVersion
                                                                                   v}.tar.gz"
          , pkgDeps = [ prelude.unbounded "guile" ]
          }

let guileConfigure =
      λ(cfg : types.BuildVars) →
          [ prelude.call
              { program = "autoreconf"
              , arguments = [ "-i" ]
              , environment = Some
                [ { var = "PATH"
                  , value =
                          prelude.mkPathVar cfg.binDirs
                      ++  ":/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
                  }
                , prelude.mkAclocalPath cfg.shareDirs
                ]
              , procDir = None Text
              }
          ]
        # prelude.defaultConfigure cfg

let guile-sqlite3 =
      λ(v : List Natural) →
          prelude.simplePackage { name = "guile-sqlite3", version = v }
        ⫽ { pkgUrl =
              "https://notabug.org/guile-sqlite3/guile-sqlite3/archive/v${prelude.showVersion
                                                                            v}.tar.gz"
          , pkgSubdir = "guile-sqlite3"
          , pkgBuildDeps =
            [ prelude.unbounded "automake"
            , prelude.unbounded "m4"
            , prelude.unbounded "pkg-config"
            , prelude.unbounded "grep"
            ]
          , pkgDeps = [ prelude.unbounded "guile", prelude.unbounded "sqlite" ]
          , configureCommand = guileConfigure
          }

let guile-gcrypt =
      λ(v : List Natural) →
          prelude.simplePackage { name = "guile-gcrypt", version = v }
        ⫽ { pkgUrl =
              "https://notabug.org/cwebber/guile-gcrypt/archive/v${prelude.showVersion
                                                                     v}.tar.gz"
          , pkgSubdir = "guile-gcrypt"
          , pkgBuildDeps =
            [ prelude.unbounded "texinfo"
            , prelude.unbounded "automake"
            , prelude.unbounded "grep"
            , prelude.unbounded "m4"
            , prelude.unbounded "pkg-config"
            ]
          , pkgDeps =
            [ prelude.unbounded "guile", prelude.unbounded "libgcrypt" ]
          , configureCommand = guileConfigure
          }

let pax =
      λ(v : Natural) →
          prelude.simplePackage { name = "pax", version = [ v ] }
        ⫽ { pkgUrl =
              "http://www.mirbsd.org/MirOS/dist/mir/cpio/paxmirabilis-${Natural/show
                                                                          v}.tgz"
          , pkgSubdir = "pax"
          , configureCommand = prelude.doNothing
          , buildCommand =
              λ(_ : types.BuildVars) →
                [ prelude.mkExe "Build.sh"
                , prelude.call
                    { program = "./Build.sh"
                    , arguments = [] : List Text
                    , environment = None (List types.EnvVar)
                    , procDir = None Text
                    }
                ]
          , installCommand =
              λ(_ : types.BuildVars) →
                  [ prelude.copyFile "pax" "bin/pax"
                  , prelude.copyFile "tar" "bin/tar"
                  , prelude.copyFile "cpio" "bin/cpio"
                  , prelude.copyFile "pax.1" "share/man/man1/pax.1"
                  , prelude.symlinkBinary "bin/pax"
                  ]
                # prelude.symlinkManpages
                    [ { file = "share/man/man1/pax.1", section = 1 } ]
          }

let cpio =
      λ(v : List Natural) →
          prelude.makeGnuExe { name = "cpio", version = v }
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/cpio/cpio-${prelude.showVersion
                                                     v}.tar.bz2"
          }

let brotli =
      λ(v : List Natural) →
          prelude.simplePackage { name = "brotli", version = v }
        ⫽ prelude.cmakePackage
        ⫽ { pkgUrl =
              "https://github.com/google/brotli/archive/v${prelude.showVersion
                                                             v}.tar.gz"
          }

let audacity =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "audacity", version = v }
            ⫽ prelude.cmakePackage
            ⫽ { pkgUrl =
                  "https://github.com/audacity/audacity/archive/Audacity-${versionString}.tar.gz"
              , pkgSubdir = "audacity-Audacity-${versionString}"
              , pkgDeps = [ prelude.unbounded "gtk2" ]
              }

let wxwidgets =
      λ(v : List Natural) →
        let versionString = prelude.showVersion v

        in    prelude.simplePackage { name = "wxWidgets", version = v }
            ⫽ { pkgUrl =
                  "https://github.com/wxWidgets/wxWidgets/releases/download/v${versionString}/wxWidgets-${versionString}.tar.bz2"
              , pkgDeps = [ prelude.unbounded "gtk3" ]
              }

let yasm =
      λ(v : List Natural) →
          prelude.simplePackage { name = "yasm", version = v }
        ⫽ { pkgUrl =
              "http://www.tortall.net/projects/yasm/releases/yasm-${prelude.showVersion
                                                                      v}.tar.gz"
          , installCommand =
              λ(cfg : types.BuildVars) →
                  prelude.installWithBinaries [ "bin/yasm" ] cfg
                # prelude.symlinkManpages
                    [ { file = "share/man/man1/yasm.1", section = 1 } ]
          }

in  [ alsa-lib [ 1, 1, 9 ]
    , apr [ 1, 7, 0 ]
    , apr-util [ 1, 6, 1 ]
    , at-spi-atk { version = [ 2, 33 ], patch = 2 }
    , at-spi-core { version = [ 2, 33 ], patch = 2 }
    , atk { version = [ 2, 36 ], patch = 0 }
    , ats [ 0, 4, 2 ]
    , audacity [ 2, 4, 2 ]
    , autoconf [ 2, 69 ]
    , automake [ 1, 16, 2 ]
    , babl { version = [ 0, 1 ], patch = 68 }
    , bash [ 5, 0, 17 ]
    , bash-completion [ 2, 9 ]
    , binutils [ 2, 35, 1 ]
    , bison [ 3, 5 ]
    , blas [ 3, 8, 0 ]
    , brotli [ 1, 0, 9 ]
    , busybox [ 1, 31, 1 ]
    , bytestructures [ 1, 0, 7 ]
    , bzip2 [ 1, 0, 8 ]
    , cairo [ 1, 16, 0 ]
    , chickenScheme [ 5, 0, 0 ]
    , cimg [ 2, 7, 0 ]
    , clang [ 9, 0, 0 ]
    , clzip [ 1, 11 ]
    , cmake { version = [ 3, 19 ], patch = 1 }
    , cmark [ 0, 29, 0 ]
    , compositeproto [ 0, 4 ]
    , coreutils [ 8, 32 ]
    , cpio [ 2, 13 ]
    , ctags [ 5, 8 ]
    , curl [ 7, 72, 0 ]
    , damageproto [ 1, 2, 1 ]
    , dbus [ 1, 13, 18 ]
    , diffutils [ 3, 7 ]
    , dri2proto [ 2, 8 ]
    , eigen [ 3, 3, 7 ]
    , elfutils [ 0, 177 ]
    , emacs [ 26, 3 ]
    , exiftool [ 11, 65 ]
    , exiv2 [ 0, 27, 1 ]
    , expat [ 2, 2, 9 ]
    , fdk-aac [ 2, 0, 1 ]
    , feh [ 3, 2, 1 ]
    , ffmpeg [ 4, 3, 1 ]
    , fftw [ 3, 3, 8 ]
    , findutils [ 4, 7, 0 ]
    , fixesproto [ 5, 0 ]
    , fontconfig [ 2, 13, 1 ]
    , fossil [ 2, 10 ]
    , flex [ 2, 6, 3 ]
    , fltk [ 1, 3, 5 ]
    , freetype-prebuild [ 2, 10, 2 ]
    , freetype [ 2, 10, 2 ]
    , fribidi [ 1, 0, 10 ]
    , gawk [ 5, 0, 1 ]
    , gc [ 8, 0, 4 ]
    , gcc [ 10, 2, 0 ]
    , gdb [ 10, 1 ]
    , gdk-pixbuf { version = [ 2, 38 ], patch = 1 }
    , gegl { version = [ 0, 4 ], patch = 16 }
    , gettext [ 0, 21 ]
    , gexiv2 { version = [ 0, 12 ], patch = 0 }
    , ghc [ 8, 10, 2 ]
    , gperf [ 3, 1 ]
    , gperftools [ 2, 7 ]
    , giflib [ 5, 2, 1 ]
    , git [ 2, 28, 0 ]
    , glib { version = [ 2, 66 ], patch = 0 }
    , glib-networking { version = [ 2, 61 ], patch = 2 }
    , glproto [ 1, 4, 17 ]
    , glu [ 9, 0, 0 ]
    , json-glib { version = [ 1, 4 ], patch = 4 }
    , glibc [ 2, 30 ]
    , gmp [ 6, 2, 0 ]
    , gobject-introspection { version = [ 1, 66 ], patch = 0 }
    , gnome-doc-utils { version = [ 0, 20 ], patch = 10 }
    , gnupg [ 2, 2, 20 ]
    , gnutls { version = [ 3, 6 ], patch = [ 15 ] }
    , graphviz [ 2, 44, 1 ]
    , grep [ 3, 5 ]
    , gsl [ 2, 6 ]
    , gtk2 { version = [ 2, 24 ], patch = 32 }
    , gtk3 { version = [ 3, 24 ], patch = 18 }
    , guile [ 3, 0, 4 ]
    , guile-gcrypt [ 0, 3, 0 ]
    , guile-git [ 0, 3, 0 ]
    , guile-json [ 4, 3, 2 ]
    , guile-sqlite3 [ 0, 1, 2 ]
    , gzip [ 1, 10 ]
    , harfbuzz [ 2, 7, 2 ]
    , htop [ 2, 2, 0 ]
    , hugs
    , icu [ 64, 2 ]
    , icu-le-hb [ 1, 0, 3 ]
    , imageMagick [ 7, 0, 8 ]
    , imlib2 [ 1, 5, 1 ]
    , inputproto [ 2, 3, 2 ]
    , intltool [ 0, 51, 0 ]
    , itstool [ 2, 0, 6 ]
    , jemalloc [ 5, 2, 1 ]
    , joe [ 4, 6 ]
    , json-c { version = [ 0, 13, 1 ], dateStr = "20180305" }
    , kbproto [ 1, 0, 7 ]
    , krb5 [ 1, 18 ]
    , lapack [ 3, 8, 0 ]
    , lcms2 [ 2, 9 ]
    , leptonica [ 1, 78, 0 ]
    , libarchive [ 3, 4, 3 ]
    , libass [ 0, 15, 0 ]
    , libassuan [ 2, 5, 3 ]
    , libatomic_ops [ 7, 6, 10 ]
    , libav [ 12, 3 ]
    , libboost [ 1, 72, 0 ]
    , libcds [ 2, 3, 2 ]
    , libcroco { version = [ 0, 6 ], patch = 12 }
    , libdatrie [ 0, 2, 12 ]
    , libdrm [ 2, 4, 102 ]
    , libepoxy [ 1, 5, 3 ]
    , libev [ 4, 25 ]
    , libevent [ 2, 1, 10 ]
    , libexif [ 0, 6, 21 ]
    , libffi [ 3, 3 ]
    , libgcrypt [ 1, 8, 6 ]
    , libgit2 [ 1, 0, 1 ]
    , libglade { version = [ 2, 6 ], patch = 4 }
    , libgpgError [ 1, 39 ]
    , libICE [ 1, 0, 10 ]
    , libiconv [ 1, 16 ]
    , libjpeg [ 9 ]
    , libjpeg-turbo [ 2, 0, 5 ]
    , libksba [ 1, 3, 5 ]
    , libmp3lame [ 3, 100 ]
    , libmypaint [ 1, 3, 0 ]
    , libnettle [ 3, 6 ]
    , libogg [ 1, 3, 4 ]
    , libopenjpeg [ 2, 3, 1 ]
    , libotf [ 0, 9, 16 ]
    , libpciaccess [ 0, 16 ]
    , libpng [ 1, 6, 37 ]
    , libpsl [ 0, 21, 0 ]
    , libpthread-stubs [ 0, 4 ]
    , libraw [ 0, 19, 2 ]
    , librsvg { version = [ 2, 48 ], patch = 4 }
    , libsamplerate [ 0, 1, 9 ]
    , libsass [ 3, 6, 2 ]
    , libselinux [ 3, 0 ]
    , libsndfile [ 1, 0, 28 ]
    , libsepol [ 3, 0 ]
    , libsodium [ 1, 0, 17 ]
    , libsoup { version = [ 2, 67 ], patch = 3 }
    , libspng [ 0, 5, 0 ]
    , libssh2 [ 1, 8, 0 ]
    , libtasn1 [ 4, 16, 0 ]
    , libtiff [ 4, 0, 10 ]
    , libtool [ 2, 4, 6 ]
    , libuv [ 1, 24, 0 ]
    , libwebp [ 1, 0, 3 ]
    , libSM [ 1, 2, 3 ]
    , libthai [ 0, 1, 28 ]
    , libvorbis [ 1, 3, 7 ]
    , libvpx [ 1, 9, 0 ]
    , libX11 [ 1, 6, 12 ]
    , libXau [ 1, 0, 9 ]
    , libXaw [ 1, 0, 13 ]
    , libXaw3d [ 1, 6, 3 ]
    , libxcb [ 1, 13 ]
    , libXcomposite [ 0, 4, 5 ]
    , libXdamage [ 1, 1, 5 ]
    , libXdmcp [ 1, 1, 3 ]
    , libXext [ 1, 3, 4 ]
    , libXfixes [ 5, 0, 3 ]
    , libXft [ 2, 3, 3 ]
    , libXi [ 1, 7, 10 ]
    , libXinerama [ 1, 1, 4 ]
    , libxml2 [ 2, 9, 9 ]
    , libXmu [ 1, 1, 3 ]
    , libXpm [ 3, 5, 13 ]
    , libXScrnSaver [ 1, 2, 3 ]
    , libxshmfence [ 1, 3 ]
    , libxslt [ 1, 1, 33 ]
    , libXrandr [ 1, 5, 2 ]
    , libXrender [ 0, 9, 10 ]
    , libXt [ 1, 2, 0 ]
    , libXtst [ 1, 2, 3 ]
    , libXxf86vm [ 1, 1, 4 ]
    , llvm [ 9, 0, 0 ]
    , lmdb [ 0, 9, 23 ]
    , lua [ 5, 4, 0 ]
    , luarocks [ 3, 3, 1 ]
    , lunzip [ 1, 11 ]
    , lz4 [ 1, 9, 2 ]
    , lzip [ 1, 21 ]
    , lziprecover [ 1, 21 ]
    , lzlib [ 1, 11 ]
    , lzo [ 2, 10 ]
    , m17n [ 1, 8, 0 ]
    , m4 [ 1, 4, 18 ]
    , make [ 4, 3 ]
    , mako [ 1, 1, 3 ]
    , markupSafe [ 1, 0 ]
    , memcached [ 1, 5, 18 ]
    , mercury
    , mesa [ 19, 0, 8 ]
    , meson [ 0, 55, 3 ]
    , mosh [ 1, 3, 2 ]
    , motif [ 2, 3, 8 ]
    , mpc [ 1, 1, 0 ]
    , mpfr [ 4, 1, 0 ]
    , mpg123 [ 1, 25, 12 ]
    , musl [ 1, 1, 20 ]
    , nano [ 4, 3 ]
    , nasm [ 2, 15, 5 ]
    , ncurses [ 6, 2 ]
    , nginx [ 1, 15, 7 ]
    , ninja [ 1, 10, 1 ]
    , node [ 14, 15, 4 ]
    , npth [ 1, 6 ]
    , nspr [ 4, 20 ]
    , openblas [ 0, 3, 2 ]
    , opencv [ 4, 2, 0 ]
    , openssh [ 7, 9 ]
    , openssl [ 1, 1, 1 ]
    , p11kit [ 0, 23, 21 ]
    , pango { version = [ 1, 43 ], patch = 0 }
    , pari [ 2, 11, 1 ]
    , patch [ 2, 7, 6 ]
    , pax 20200904
    , pcre [ 8, 44 ]
    , pcre2 [ 10, 35 ]
    , pdfgrep [ 2, 1, 2 ]
    , perl5 [ 5, 30, 2 ]
    , phash [ 0, 9, 6 ]
    , pixman [ 0, 40, 0 ]
    , pkg-config [ 0, 29, 2 ]
    , poppler [ 20, 9, 0 ]
    , postgresql [ 12, 2 ]
    , protobuf [ 3, 8, 0 ]
    , pycairo [ 1, 20, 0 ]
    , pygobject { version = [ 2, 28 ], patch = 7 }
    , pygtk { version = [ 2, 24 ], patch = 0 }
    , python [ 2, 7, 17 ]
    , python [ 3, 9, 0 ]
    , qrencode [ 4, 1, 0 ]
    , qt { version = [ 5, 13 ], patch = 0 }
    , quazip [ 0, 8, 1 ]
    , r [ 4, 0, 2 ]
    , ragel [ 6, 10 ]
    , randrproto [ 1, 5, 0 ]
    , re2c [ 2, 0 ]
    , readline [ 8, 0 ]
    , recordproto [ 1, 14, 2 ]
    , renderproto [ 0, 11, 1 ]
    , rlwrap [ 0, 44 ]
    , ruby { version = [ 2, 7 ], patch = 1 }
    , rustc [ 1, 46, 0 ]
    , scour [ 0, 37 ]
    , scrnsaverproto [ 1, 2, 2 ]
    , sdl2 [ 2, 0, 10 ]
    , sed [ 4, 7 ]
    , shared-mime-info [ 1, 10 ]
    , smake [ 1, 2, 5 ]
    , sqlite { version = [ 3, 33, 0 ] }
    , star [ 1, 6 ]
    , subversion [ 1, 12, 2 ]
    , swig [ 3, 0, 12 ]
    , swi-prolog [ 8, 0, 3 ]
    , tar [ 1, 32 ]
    , tarlz [ 0, 17 ]
    , tcc [ 0, 9, 27 ]
    , texinfo [ 6, 7 ]
    , tesseract [ 4, 0, 0 ]
    , time [ 1, 9 ]
    , unistring [ 0, 9, 10 ]
    , utf8proc [ 2, 4, 0 ]
    , util-linux { version = [ 2, 36 ] }
    , util-macros [ 1, 19, 2 ]
    , vala { version = [ 0, 45 ], patch = 3 }
    , valgrind [ 3, 16, 1 ]
    , vim [ 8, 2 ]
    , wayland [ 1, 17, 0 ]
    , wget [ 1, 20, 3 ]
    , which [ 2, 21 ]
    , wxwidgets [ 3, 1, 4 ]
    , xcb-proto [ 1, 13 ]
    , xextproto [ 7, 3, 0 ]
    , xf86vidmodeproto [ 2, 3, 1 ]
    , xineramaproto [ 1, 2 ]
    , xmlParser [ 2, 44 ]
    , xproto [ 7, 0, 31 ]
    , xtrans [ 1, 4, 0 ]
    , xz [ 5, 2, 5 ]
    , yasm [ 1, 3, 0 ]
    , zlib [ 1, 2, 11 ]
    , zstd [ 1, 4, 5 ]
    ]
