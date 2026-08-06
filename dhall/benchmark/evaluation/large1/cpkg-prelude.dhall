{- Dhall prelude imports -}
let concatMapSep =
      ./Prelude/Text/concatMapSep

let concatMapText =
      ./Prelude/Text/concatMap

let concatMap =
      ./Prelude/List/concatMap

let map =
      ./Prelude/List/map

let mapOptional =
      ./Prelude/Optional/map


let not = ./Prelude/Bool/not


let types = ./cpkg-types.dhall

let showVersion = concatMapSep "." Natural Natural/show

let maybeAppend =
      λ(a : Type) →
      λ(x : Optional a) →
      λ(xs : List a) →
        merge { Some = λ(x : a) → xs # [ x ], None = xs } x

let printArch =
      λ(arch : types.Arch) →
        merge
          { X64 = "x86_64"
          , AArch = "aarch64"
          , Arm = "arm"
          , RISCV64 = "riscv64"
          , PowerPC = "powerpc"
          , PowerPC64 = "powerpc64"
          , PowerPC64le = "powerpc64le"
          , Sparc64 = "sparc64"
          , S390x = "s390x"
          , Alpha = "alpha"
          , M68k = "m68k"
          , Mips = "mips"
          , MipsEl = "mipsel"
          , Mips64 = "mips64"
          , Mips64El = "mips64el"
          , X86 = "i686"
          , SH4 = "sh4"
          , HPPA = "hppa"
          , HPPA64 = "hppa64"
          , MipsIsa32r6El = "mipsisa32r6el"
          , MipsIsa32r6 = "mipsisa32r6"
          , MipsIsa64r6El = "mipsisa64r6el"
          , MipsIsa64r6 = "mipsisa64r6"
          }
          arch

let printManufacturer =
      λ(x : types.Manufacturer) →
        merge { Unknown = "unknown", Apple = "apple", IBM = "ibm", PC = "pc" } x

let libSuffix =
      λ(os : types.OS) →
        merge
          { FreeBSD = "so"
          , OpenBSD = "so"
          , NetBSD = "so"
          , Solaris = "so"
          , Dragonfly = "so"
          , Linux = "so"
          , Darwin = "dylib"
          , Windows = "dll"
          , Redox = "so"
          , Haiku = "so"
          , IOS = "dylib"
          , AIX = "so"
          , Hurd = "so"
          , Android = "so"
          , NoOs = "so"
          }
          os

let printOS =
      λ(os : types.OS) →
        merge
          { FreeBSD = "freebsd"
          , OpenBSD = "openbsd"
          , NetBSD = "netbsd"
          , Solaris = "solaris"
          , Dragonfly = "dragonfly"
          , Linux = "linux"
          , Darwin = "darwin"
          , Windows = "w64"
          , Redox = "redox"
          , Haiku = "haiku"
          , IOS = "darwin"
          , AIX = "aix"
          , Hurd = "hurd"
          , Android = "android"
          , NoOs = "none"
          }
          os

let printABI =
      λ(os : types.ABI) →
        merge
          { GNU = "gnu"
          , GNUabi64 = "gnuabi64"
          , GNUeabi = "gnueabi"
          , GNUeabihf = "gnueabihf"
          , GNUspe = "gnuspe"
          , MinGw = "mingw32"
          }
          os

let printTargetTriple =
      λ(t : types.TargetTriple) →
            "${printArch t.arch}-${printOS t.os}"
        ++  merge
              { Some = λ(abi : types.ABI) → "-${printABI abi}", None = "" }
              t.abi

let mkHost =
      mapOptional
        types.TargetTriple
        Text
        (λ(tgt : types.TargetTriple) → "--host=${printTargetTriple tgt}")

let makeExe =
      λ(os : types.OS) →
        merge
          { FreeBSD = "gmake"
          , OpenBSD = "gmake"
          , NetBSD = "gmake"
          , Solaris = "gmake"
          , Dragonfly = "gmake"
          , Linux = "make"
          , Darwin = "make"
          , Windows = "make"
          , Redox = "make"
          , Haiku = "make"
          , IOS = "make"
          , AIX = "make"
          , Hurd = "make"
          , Android = "make"
          , NoOs = "make"
          }
          os

let mkExe = λ(x : Text) → types.Command.MakeExecutable { file = x }

let mkExes = map Text types.Command mkExe

let writeFile = types.Command.Write

let patch = λ(x : Text) → types.Command.Patch { patchContents = x }

let defaultEnv = None (List types.EnvVar)

let defaultCall =
      { arguments = [] : List Text
      , environment = defaultEnv
      , procDir = None Text
      }

let call = types.Command.Call

let symlinkBinary = λ(file : Text) → types.Command.SymlinkBinary { file }

let symlinkManpage = types.Command.SymlinkManpage

let symlink =
      λ(tgt : Text) →
      λ(lnk : Text) →
        types.Command.Symlink { tgt, linkName = lnk }

let copyFile =
      λ(src : Text) → λ(dest : Text) → types.Command.CopyFile { src, dest }

let symlinkBinaries = map Text types.Command symlinkBinary

let symlinkManpages =
      map { file : Text, section : Natural } types.Command symlinkManpage

let isUnix =
      λ(os : types.OS) →
        merge
          { FreeBSD = True
          , OpenBSD = True
          , NetBSD = True
          , Solaris = True
          , Dragonfly = True
          , Linux = True
          , Darwin = True
          , Windows = False
          , Redox = False
          , Haiku = False
          , IOS = True
          , AIX = True
          , Hurd = True
          , Android = True
          , NoOs = False
          }
          os

let isMac =
      λ(os : types.OS) →
        merge
          { FreeBSD = False
          , OpenBSD = False
          , NetBSD = False
          , Solaris = False
          , Dragonfly = False
          , Linux = False
          , Darwin = True
          , Windows = False
          , Redox = False
          , Haiku = False
          , IOS = False
          , AIX = False
          , Hurd = False
          , Android = False
          , NoOs = False
          }
          os

let mkLDFlagsGeneral =
      λ(libDirs : List Text) →
      λ(linkLibs : List Text) →
      λ(cfg : types.BuildVars) →
        let flag0 = concatMapSep " " Text (λ(dir : Text) → "-L${dir}") libDirs

        let flag1 = concatMapText Text (λ(dir : Text) → " -l${dir}") linkLibs

        let flag2 =
              concatMapText
                Text
                ( λ(dir : Text) →
                    if isMac cfg.buildOS then "" else " -Wl,-rpath-link,${dir}"
                )
                libDirs

        in  { var = "LDFLAGS", value = flag0 ++ flag1 ++ flag2 }

let mkLDFlags =
      λ(libDirs : List Text) → mkLDFlagsGeneral libDirs ([] : List Text)

let mkLDPath =
      λ(libDirs : List Text) →
        let flag = concatMapSep ":" Text (λ(dir : Text) → dir) libDirs

        in  { var = "LD_LIBRARY_PATH", value = flag }

let mkLDRunPath =
      λ(libDirs : List Text) →
        let flag = concatMapSep ":" Text (λ(dir : Text) → dir) libDirs

        in  { var = "LD_RUN_PATH", value = flag }

let mkStaPath =
      λ(libDirs : List Text) →
        let flag = concatMapText Text (λ(dir : Text) → "${dir}:") libDirs

        in  { var = "LIBRARY_PATH"
            , value = flag ++ "/usr/local/lib:/lib:/usr/lib"
            }

let osCfg =
      λ(cfg : types.BuildVars) →
        merge
          { Some = λ(tgt : types.TargetTriple) → tgt.os, None = cfg.buildOS }
          cfg.targetTriple

let archCfg =
      λ(cfg : types.BuildVars) →
        merge
          { Some = λ(tgt : types.TargetTriple) → tgt.arch
          , None = cfg.buildArch
          }
          cfg.targetTriple

let mkPerlLib =
      λ ( x
        : { libDirs : List Text
          , perlVersion : List Natural
          , cfg : types.BuildVars
          }
        ) →
        let os = x.cfg.buildOS

        let arch = x.cfg.buildArch

        let flag =
              concatMapSep
                ":"
                Text
                ( λ(dir : Text) →
                    "${dir}/site_perl/${showVersion
                                          x.perlVersion}/${printArch
                                                             arch}-${printOS
                                                                       os}/"
                )
                x.libDirs

        let major =
              merge
                { Some = Natural/show, None = "" }
                (List/head Natural x.perlVersion)

        in  { var = "PERL${major}LIB", value = flag }

let mkIncludePath =
      λ(incls : List Text) →
        let flag = concatMapSep ":" Text (λ(dir : Text) → dir) incls

        in  { var = "C_INCLUDE_PATH", value = flag }

let mkCFlags =
      λ(cfg : types.BuildVars) →
        let flag =
              concatMapSep " " Text (λ(dir : Text) → "-I${dir}") cfg.includeDirs

        let staFlag = if cfg.static then " -static" else ""

        in  { var = "CPPFLAGS", value = flag ++ staFlag }

let mkPkgConfigVar =
      λ(libDirs : List Text) →
        let flag =
              concatMapSep ":" Text (λ(dir : Text) → "${dir}/pkgconfig") libDirs

        in  { var = "PKG_CONFIG_PATH", value = flag }

let mkXdgDataDirs =
      λ(shareDirs : List Text) →
        let flag = concatMapSep ":" Text (λ(dir : Text) → dir) shareDirs

        in  { var = "XDG_DATA_DIRS", value = flag }

let mkPathVar =
      λ(binDirs : List Text) →
        concatMapText Text (λ(dir : Text) → "${dir}:") binDirs

let unixPath =
      λ(binDirs : List Text) →
        { var = "PATH"
        , value =
                mkPathVar binDirs
            ++  "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
        }

let defaultPath =
      λ(cfg : types.BuildVars) →
        if    isUnix cfg.buildOS
        then  [ unixPath cfg.binDirs ] : List types.EnvVar
        else  [] : List types.EnvVar

let libPath =
      λ(cfg : types.BuildVars) →
        if cfg.static then mkStaPath cfg.linkDirs else mkLDPath cfg.linkDirs

let mkLDPreload =
      λ(libs : List Text) →
        let flag = concatMapSep " " Text (λ(lib : Text) → lib) libs

        in  { var = "LD_PRELOAD", value = flag }

let mkGuileEnv =
      λ(libs : List Text) →
      λ(share : List Text) →
        let flag0 =
              concatMapSep
                ":"
                Text
                (λ(lib : Text) → "${lib}/guile/3.0/site-ccache")
                libs

        let flag1 =
              concatMapSep
                ":"
                Text
                (λ(dir : Text) → "${dir}/guile/site/3.0")
                share

        in  [ { var = "GUILE_LOAD_COMPILED_PATH", value = flag0 }
            , { var = "GUILE_LOAD_PATH", value = flag1 }
            ]

let configEnv =
      λ(linkLibs : List Text) →
      λ(cfg : types.BuildVars) →
          defaultPath cfg
        # [ mkLDFlagsGeneral cfg.linkDirs linkLibs cfg
          , mkCFlags cfg
          , mkPkgConfigVar (cfg.shareDirs # cfg.linkDirs)
          , libPath cfg
          , mkLDRunPath cfg.linkDirs
          , mkPerlLib
              { libDirs = cfg.linkDirs, perlVersion = [ 5, 30, 2 ], cfg }
          ]
        # mkGuileEnv cfg.linkDirs cfg.shareDirs

let buildEnv =
      λ(cfg : types.BuildVars) →
          defaultPath cfg
        # [ mkPkgConfigVar (cfg.shareDirs # cfg.linkDirs)
          , mkPerlLib
              { libDirs = cfg.linkDirs, perlVersion = [ 5, 30, 2 ], cfg }
          , mkLDPath cfg.linkDirs
          , mkLDFlagsGeneral cfg.linkDirs ([] : List Text) cfg
          ]
        # mkGuileEnv cfg.linkDirs cfg.shareDirs

let configSome =
      λ(linkLibs : List Text) →
      λ(cfg : types.BuildVars) →
        Some (configEnv linkLibs cfg)

let generalConfigure =
      λ(envVars : List Text → types.BuildVars → Optional (List types.EnvVar)) →
      λ(filename : Text) →
      λ(linkLibs : List Text) →
      λ(extraFlags : List Text) →
      λ(cfg : types.BuildVars) →
        let maybeHost = mkHost cfg.targetTriple

        let modifyArgs = maybeAppend Text maybeHost

        let mac = isMac cfg.buildOS

        let modifyProg =
              if    mac
              then  λ(x : List Text) → [ "${filename}" ] # x
              else  λ(x : List Text) → x

        let program = if mac then "sh" else "./${filename}"

        in  [ call
                (   defaultCall
                  ⫽ { program
                    , arguments =
                        modifyProg
                          (   modifyArgs [ "--prefix=${cfg.installDir}" ]
                            # extraFlags
                          )
                    , environment = envVars linkLibs cfg
                    }
                )
            ]

let configWithEnv =
      λ(envVars : List Text → types.BuildVars → Optional (List types.EnvVar)) →
        generalConfigure envVars "configure" ([] : List Text) ([] : List Text)

let configureWithFlags =
      generalConfigure configSome "configure" ([] : List Text)

let defaultConfigure = configureWithFlags ([] : List Text)

let configureLinkExtraLibs =
      λ(linkLibs : List Text) →
        generalConfigure configSome "configure" linkLibs ([] : List Text)

let mkAclocalPath =
      λ(shareDirs : List Text) →
        let flag =
              concatMapSep
                ":"
                Text
                (λ(dir : Text) → "${dir}/aclocal:${dir}/autoconf")
                shareDirs

        in  { var = "ACLOCAL_PATH", value = flag }

let configureMkExesExtraFlags =
      λ(x : { bins : List Text, extraFlags : List Text }) →
      λ(cfg : types.BuildVars) →
        mkExes x.bins # configureWithFlags x.extraFlags cfg

let configureMkExes =
      λ(bins : List Text) →
        configureMkExesExtraFlags { bins, extraFlags = [] : List Text }

let generalBuild =
      λ(cpus : types.BuildVars → Natural) →
      λ(envs : List types.EnvVar) →
      λ(cfg : types.BuildVars) →
        [ call
            (   defaultCall
              ⫽ { program = makeExe cfg.buildOS
                , arguments = [ "-j${Natural/show (cpus cfg)}" ]
                , environment = Some envs
                }
            )
        ]

let defaultCpus = λ(cfg : types.BuildVars) → cfg.cpus

let singleThreaded = λ(_ : types.BuildVars) → 1

let buildWith = generalBuild defaultCpus

let defaultBuild = λ(cfg : types.BuildVars) → buildWith (buildEnv cfg) cfg

let installWith =
      λ(envs : List types.EnvVar) →
      λ(cfg : types.BuildVars) →
        [ call
            (   defaultCall
              ⫽ { program = makeExe cfg.buildOS
                , arguments = [ "install" ]
                , environment = Some envs
                }
            )
        ]

let defaultInstall = λ(cfg : types.BuildVars) → installWith (buildEnv cfg) cfg

let installWithBinaries =
      λ(bins : List Text) →
      λ(installVars : types.BuildVars) →
          defaultInstall installVars
        # ( if    not installVars.isCross
            then  symlinkBinaries bins
            else  [] : List types.Command
          )

let installWithManpages =
      λ(mans : List { file : Text, section : Natural }) →
      λ(installVars : types.BuildVars) →
          defaultInstall installVars
        # ( if    not installVars.isCross
            then  symlinkManpages mans
            else  [] : List types.Command
          )

let unbounded = λ(x : Text) → { name = x, bound = types.VersionBound.NoBound }

let lowerBound =
      λ(pkg : { name : Text, lower : List Natural }) →
        { name = pkg.name
        , bound = types.VersionBound.Lower { lower = pkg.lower }
        }

let upperBound =
      λ(pkg : { name : Text, upper : List Natural }) →
        { name = pkg.name
        , bound = types.VersionBound.Upper { upper = pkg.upper }
        }

let defaultPackage =
      { configureCommand = defaultConfigure
      , buildCommand = defaultBuild
      , installCommand = defaultInstall
      , pkgBuildDeps = [] : List types.Dep
      , pkgDeps = [] : List types.Dep
      }

let simplePackage =
      λ(pkg : { name : Text, version : List Natural }) →
          defaultPackage
        ⫽ { pkgName = pkg.name
          , pkgVersion = pkg.version
          , pkgSubdir = "${pkg.name}-${showVersion pkg.version}"
          }

let makeGnuExe =
      λ(pkg : { name : Text, version : List Natural }) →
          simplePackage pkg
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/gnu/${pkg.name}/${pkg.name}-${showVersion
                                                                   pkg.version}.tar.xz"
          , installCommand = installWithBinaries [ "bin/${pkg.name}" ]
          }

let makeGnuLibrary =
      λ(pkg : { name : Text, version : List Natural }) →
          simplePackage pkg
        ⫽ { pkgUrl =
              "https://ftp.gnu.org/pub/gnu/lib${pkg.name}/lib${pkg.name}-${showVersion
                                                                             pkg.version}.tar.xz"
          , pkgSubdir = "lib${pkg.name}-${showVersion pkg.version}"
          }

let createDir = λ(x : Text) → types.Command.CreateDirectory { dir = x }

let printCMakeOS =
      λ(os : types.OS) →
        merge
          { FreeBSD = "BSD"
          , OpenBSD = "BSD"
          , NetBSD = "BSD"
          , Solaris = "Solaris"
          , Dragonfly = "BSD"
          , Linux = "Linux"
          , Darwin = "Darwin"
          , Windows = "Windows"
          , Redox = "Redox"
          , Haiku = "Haiku"
          , IOS = "Darwin"
          , AIX = "AIX"
          , Hurd = "Hurd"
          , Android = "Android"
          , NoOs = "Generic"
          }
          os

let cmakeConfigureGeneral =
      λ(envVars : types.BuildVars → Optional (List types.EnvVar)) →
      λ(flags : List Text) →
      λ(cfg : types.BuildVars) →
        let host =
              merge
                { Some =
                    λ(tgt : types.TargetTriple) →
                      [ "-DCMAKE_C_COMPILER=${printTargetTriple tgt}-gcc"
                      , "-DCMAKE_CXX_COMPILER=${printTargetTriple tgt}-g++"
                      ]
                , None =
                  [ "-DCMAKE_C_COMPILER=gcc", "-DCMAKE_CXX_COMPILER=g++" ]
                }
                cfg.targetTriple

        let system =
              merge
                { Some =
                    λ(tgt : types.TargetTriple) →
                      [ "-DCMAKE_SYSTEM_NAME=${printCMakeOS tgt.os}" ]
                , None = [] : List Text
                }
                cfg.targetTriple

        in  [ createDir "build"
            , call
                { program = "cmake"
                , arguments =
                      [ "../"
                      , "-DCMAKE_INSTALL_PREFIX:PATH=${cfg.installDir}"
                      , "-DCMAKE_MAKE_PROGRAM=${makeExe cfg.buildOS}"
                      ]
                    # host
                    # system
                    # flags
                , environment = envVars cfg
                , procDir = Some "build"
                }
            ]

let cmakeEnv =
      λ(cfg : types.BuildVars) →
          [ mkPkgConfigVar (cfg.shareDirs # cfg.linkDirs)
          , { var = "CMAKE_INCLUDE_PATH"
            , value = (mkIncludePath cfg.includeDirs).value
            }
          , { var = "CMAKE_LIBRARY_PATH", value = (libPath cfg).value }
          ]
        # defaultPath cfg

let cmakeSome = λ(cfg : types.BuildVars) → Some (cmakeEnv cfg)

let cmakeConfigureWithFlags = cmakeConfigureGeneral cmakeSome

let cmakeConfigure = cmakeConfigureWithFlags ([] : List Text)

let cmakeConfigureNinja =
      λ(cfg : types.BuildVars) →
        let host =
              merge
                { Some =
                    λ(tgt : types.TargetTriple) →
                      [ "-DCMAKE_C_COMPILER=${printTargetTriple tgt}-gcc"
                      , "-DCMAKE_CXX_COMPILER=${printTargetTriple tgt}-g++"
                      ]
                , None = [] : List Text
                }
                cfg.targetTriple

        let system =
              merge
                { Some =
                    λ(tgt : types.TargetTriple) →
                      [ "-DCMAKE_SYSTEM_NAME=${printCMakeOS tgt.os}" ]
                , None = [] : List Text
                }
                cfg.targetTriple

        in  [ createDir "build"
            , call
                { program = "cmake"
                , arguments =
                      [ "../"
                      , "-DCMAKE_INSTALL_PREFIX:PATH=${cfg.installDir}"
                      , "-G"
                      , "Ninja"
                      ]
                    # host
                    # system
                , environment = Some (cmakeEnv cfg)
                , procDir = Some "build"
                }
            ]

let cmakeBuild =
      λ(cfg : types.BuildVars) →
        [ call
            { program = "cmake"
            , arguments =
              [ "--build"
              , "."
              , "--config"
              , "Release"
              , "--"
              , "-j"
              , Natural/show cfg.cpus
              ]
            , environment = Some (cmakeEnv cfg)
            , procDir = Some "build"
            }
        ]

let cmakeInstall =
      λ(cfg : types.BuildVars) →
        [ call
            { program = "cmake"
            , arguments =
              [ "--build", ".", "--target", "install", "--config", "Release" ]
            , environment = Some (cmakeEnv cfg)
            , procDir = Some "build"
            }
        ]

let cmakeInstallWithBinaries =
      λ(bins : List Text) →
      λ(installVars : types.BuildVars) →
        cmakeInstall installVars # symlinkBinaries bins

let cmakePackage =
        defaultPackage
      ⫽ { configureCommand = cmakeConfigure
        , buildCommand = cmakeBuild
        , installCommand = cmakeInstall
        , pkgBuildDeps = [ unbounded "cmake" ]
        }

let autogenConfigure =
      λ(cfg : types.BuildVars) →
          [ call
              (   defaultCall
                ⫽ { program = "./autogen.sh"
                  , environment = Some
                      (   [ mkAclocalPath cfg.shareDirs
                          , mkPkgConfigVar (cfg.shareDirs # cfg.linkDirs)
                          ]
                        # defaultPath cfg
                      )
                  }
              )
          ]
        # defaultConfigure cfg

let fullVersion =
      λ(x : { version : List Natural, patch : Natural }) →
        x.version # [ x.patch ]

let mkPyPath =
      λ(version : List Natural) →
      λ(libDirs : List Text) →
        let flag =
              concatMapSep
                ":"
                Text
                ( λ(dir : Text) →
                    "${dir}/python${showVersion version}/site-packages"
                )
                libDirs

        in  { var = "PYTHONPATH", value = flag }

let mkPy3Path = mkPyPath [ 3, 9 ]

let mesonCfgFile =
      λ(cfg : types.BuildVars) →
        let prefix =
              merge
                { Some =
                    λ(tgt : types.TargetTriple) → "${printTargetTriple tgt}-"
                , None = ""
                }
                cfg.targetTriple

        in      ''
                [binaries]
                ''
            ++  ''
                c = '${prefix}gcc'
                ''
            ++  ''
                cpp = '${prefix}g++'
                ''
            ++  ''
                ar = '${prefix}ar'
                ''
            ++  ''
                strip = '${prefix}strip'
                ''
            ++  ''
                pkgconfig = 'pkg-config'
                ''
            ++  ''
                [host_machine]
                ''
            ++  ''
                system = '${printOS (osCfg cfg)}'
                ''
            ++  ''
                cpu_family = '${printArch (archCfg cfg)}'
                ''
            ++  ''
                cpu = '${printArch (archCfg cfg)}'
                ''
            ++  "endian = 'little'"

let mesonEnv =
      λ(cfg : types.BuildVars) →
        Some
          (   [ mkPkgConfigVar (cfg.linkDirs # cfg.shareDirs)
              , mkPy3Path cfg.linkDirs
              , libPath cfg
              , mkLDRunPath cfg.linkDirs
              , mkLDFlags cfg.linkDirs cfg
              , mkCFlags cfg
              , mkLDPreload cfg.preloadLibs
              ]
            # defaultPath cfg
          )

let mesonConfigureGeneral =
      λ(envs : types.BuildVars → Optional (List types.EnvVar)) →
      λ(flags : List Text) →
      λ(cfg : types.BuildVars) →
        let crossArgs =
              if    cfg.isCross
              then  [ "--cross-file", "cross.txt" ]
              else  [] : List Text

        in  [ createDir "build"
            , writeFile
                { file = "build/cross.txt", contents = mesonCfgFile cfg }
            , call
                { program = "meson"
                , arguments =
                    [ "--prefix=${cfg.installDir}", ".." ] # crossArgs # flags
                , environment = envs cfg
                , procDir = Some "build"
                }
            ]

let mesonConfigureWithFlags = mesonConfigureGeneral mesonEnv

let mesonConfigure = mesonConfigureWithFlags ([] : List Text)

let ninjaBuildWith =
      λ(linkLibs : List Text) →
      λ(cfg : types.BuildVars) →
        let ldPreload =
              if    cfg.isCross
              then  [] : List types.EnvVar
              else  [ mkLDPreload cfg.preloadLibs ]

        in  [ call
                (   defaultCall
                  ⫽ { program = "ninja"
                    , environment = Some
                        (   [ mkPkgConfigVar cfg.linkDirs
                            , mkPy3Path cfg.linkDirs
                            , libPath cfg
                            , mkLDRunPath cfg.linkDirs
                            , mkLDFlagsGeneral cfg.linkDirs linkLibs cfg
                            , mkCFlags cfg
                            ]
                          # defaultPath cfg
                          # ldPreload
                        )
                    , procDir = Some "build"
                    }
                )
            ]

let ninjaBuild = ninjaBuildWith ([] : List Text)

let ninjaInstall =
      λ(cfg : types.BuildVars) →
        [ call
            (   defaultCall
              ⫽ { program = "ninja"
                , environment = Some
                    (   [ mkPkgConfigVar cfg.linkDirs
                        , mkPy3Path cfg.linkDirs
                        , libPath cfg
                        , mkLDRunPath cfg.linkDirs
                        , mkLDFlags cfg.linkDirs cfg
                        , mkCFlags cfg
                        ]
                      # defaultPath cfg
                    )
                , arguments = [ "install" ]
                , procDir = Some "build"
                }
            )
        ]

let ninjaPackage =
      λ(x : { name : Text, version : List Natural }) →
          simplePackage x
        ⫽ { configureCommand = mesonConfigure
          , buildCommand = ninjaBuild
          , installCommand = ninjaInstall
          , pkgBuildDeps = [ unbounded "meson", unbounded "ninja" ]
          }

let copyFiles =
      map { src : Text, dest : Text } types.Command types.Command.CopyFile

let ninjaInstallWithPkgConfig =
      λ(fs : List { src : Text, dest : Text }) →
      λ(cfg : types.BuildVars) →
        ninjaInstall cfg # copyFiles fs

let doNothing = λ(_ : types.BuildVars) → [] : List types.Command

let mesonMoves =
      map
        Text
        { src : Text, dest : Text }
        ( λ(pcFile : Text) →
            { src = "build/meson-private/${pcFile}"
            , dest = "lib/pkgconfig/${pcFile}"
            }
        )

let pythonBuild =
      λ(version : List Natural) →
      λ(cfg : types.BuildVars) →
        let major =
              merge
                { Some = Natural/show, None = "" }
                (List/head Natural version)

        let versionString = showVersion version

        in  [ createDir
                "${cfg.installDir}/lib/python${versionString}/site-packages"
            , call
                (   defaultCall
                  ⫽ { program = "python${major}"
                    , arguments = [ "setup.py", "build" ]
                    , environment = Some
                        (   [ { var = "PYTHONPATH"
                              , value =
                                  "${cfg.installDir}/lib/python${versionString}/site-packages"
                              }
                            , mkPkgConfigVar cfg.linkDirs
                            , libPath cfg
                            ]
                          # defaultPath cfg
                        )
                    }
                )
            ]

let pythonInstall =
      λ(version : List Natural) →
      λ(cfg : types.BuildVars) →
        let major =
              merge
                { Some = Natural/show, None = "" }
                (List/head Natural version)

        let versionString = showVersion version

        in  [ createDir
                "${cfg.installDir}/lib/python${versionString}/site-packages"
            , call
                (   defaultCall
                  ⫽ { program = "python${major}"
                    , arguments =
                      [ "setup.py"
                      , "install"
                      , "--prefix=${cfg.installDir}"
                      , "--optimize=1"
                      ]
                    , environment = Some
                        (   [ { var = "PYTHONPATH"
                              , value =
                                  "${cfg.installDir}/lib/python${versionString}/site-packages"
                              }
                            , mkPkgConfigVar cfg.linkDirs
                            , libPath cfg
                            ]
                          # defaultPath cfg
                        )
                    }
                )
            ]

let pythonPackage =
      λ(pyVersion : List Natural) →
      λ(x : { name : Text, version : List Natural }) →
        let major =
              merge
                { Some = Natural/show, None = "" }
                (List/head Natural pyVersion)

        in    simplePackage x
            ⫽ { configureCommand = doNothing
              , buildCommand = pythonBuild pyVersion
              , installCommand = pythonInstall pyVersion
              , pkgBuildDeps = [ unbounded "python${major}" ]
              }

let python3Build = pythonBuild [ 3, 9 ]

let python3Install = pythonInstall [ 3, 9 ]

let python3Package = pythonPackage [ 3, 9 ]

let python2Package = pythonPackage [ 2, 7 ]

let mkCCVar =
      λ(cfg : types.BuildVars) →
        merge
          { Some =
              λ(tgt : types.TargetTriple) →
                [ { var = "CC", value = "${printTargetTriple tgt}-gcc" } ]
          , None = [] : List types.EnvVar
          }
          cfg.targetTriple

let squishVersion = concatMapText Natural Natural/show

let mkCCArg =
      λ(cfg : types.BuildVars) →
        merge
          { Some =
              λ(tgt : types.TargetTriple) →
                [ "CC=${printTargetTriple tgt}-gcc" ]
          , None = [] : List Text
          }
          cfg.targetTriple

let mkFRCArg =
      λ(cfg : types.BuildVars) →
        merge
          { Some =
              λ(tgt : types.TargetTriple) →
                [ "CC=${printTargetTriple tgt}-gcc" ]
          , None = [] : List Text
          }
          cfg.targetTriple

let preloadEnv =
      λ(_ : List Text) →
      λ(cfg : types.BuildVars) →
        Some
          (   defaultPath cfg
            # [ mkLDFlags cfg.linkDirs cfg
              , mkCFlags cfg
              , mkPkgConfigVar cfg.linkDirs
              , libPath cfg
              , mkXdgDataDirs cfg.shareDirs
              , mkLDPreload cfg.preloadLibs
              , mkPerlLib
                  { libDirs = cfg.linkDirs, perlVersion = [ 5, 30, 2 ], cfg }
              ]
          )

let perlConfigure =
      λ(cfg : types.BuildVars) →
        [ call
            { program = "perl"
            , arguments = [ "Makefile.PL", "PREFIX=${cfg.installDir}" ]
            , environment = preloadEnv ([] : List Text) cfg
            , procDir = None Text
            }
        ]

let preloadCfg =
      generalConfigure preloadEnv "configure" ([] : List Text) ([] : List Text)

let printEnvVar = λ(var : types.EnvVar) → "${var.var}=${var.value}"

let mkPyWrapper =
      λ(version : List Natural) →
      λ(binName : Text) →
      λ(cfg : types.BuildVars) →
        let wrapperContents =
              "${printEnvVar
                   ( libPath cfg
                   )} ${printEnvVar
                          ( mkPyPath version cfg.linkDirs
                          )}:${cfg.installDir}/lib/python${showVersion
                                                             version}/site-packages ${cfg.installDir}/bin/${binName} \$@"

        let wrapped = "wrapper/${binName}"

        in  [ createDir "wrapper"
            , writeFile { file = wrapped, contents = wrapperContents }
            , mkExe wrapped
            , copyFile wrapped wrapped
            , symlinkBinary wrapped
            ]

let mkPy3Wrapper = mkPyWrapper [ 3, 9 ]

let mkPy2Wrapper = mkPyWrapper [ 2, 7 ]

let installWithPyWrappers =
      λ(version : List Natural) →
      λ(binNames : List Text) →
      λ(cfg : types.BuildVars) →
          pythonInstall version cfg
        # concatMap
            Text
            types.Command
            (λ(bin : Text) → mkPyWrapper version bin cfg)
            binNames

let installWithPy3Wrappers = installWithPyWrappers [ 3, 9 ]

let mkLDPathWrapper =
      λ(cfg : types.BuildVars) →
      λ(binName : Text) →
        let wrapper =
              "${printEnvVar
                   ( mkLDPath cfg.linkDirs
                   )}:${cfg.installDir}/lib LD_PRELOAD='${( mkLDPreload
                                                              cfg.preloadLibs
                                                          ).value}' ${cfg.installDir}/bin/${binName} \$@"

        let wrapped = "wrapper/${binName}"

        in  [ createDir "wrapper"
            , writeFile { file = wrapped, contents = wrapper }
            , mkExe wrapped
            , copyFile wrapped wrapped
            , symlinkBinary wrapped
            ]

let mkLDPathWrappers =
      λ(cfg : types.BuildVars) →
      λ(bins : List Text) →
        concatMap
          Text
          types.Command
          (λ(bin : Text) → mkLDPathWrapper cfg bin)
          bins

let installWithWrappers =
      λ(bins : List Text) →
      λ(cfg : types.BuildVars) →
        defaultInstall cfg # mkLDPathWrappers cfg bins

let underscoreVersion = concatMapSep "_" Natural Natural/show

let isX64 =
      λ(arch : types.Arch) →
        merge
          { X64 = True
          , AArch = False
          , Arm = False
          , RISCV64 = False
          , PowerPC = False
          , PowerPC64 = False
          , PowerPC64le = False
          , Sparc64 = False
          , S390x = False
          , Alpha = False
          , M68k = False
          , Mips = False
          , MipsEl = False
          , Mips64 = False
          , Mips64El = False
          , X86 = False
          , SH4 = False
          , HPPA = False
          , HPPA64 = False
          , MipsIsa32r6El = False
          , MipsIsa32r6 = False
          , MipsIsa64r6El = False
          , MipsIsa64r6 = False
          }
          arch

let configureWithPatches =
      λ(patches : List Text) →
      λ(cfg : types.BuildVars) →
          map Text types.Command (λ(p : Text) → patch p) patches
        # defaultConfigure cfg

let configureWithPatch = λ(p : Text) → configureWithPatches [ p ]

let installPrefix =
      λ(cfg : types.BuildVars) →
        [ call
            (   defaultCall
              ⫽ { program = "make"
                , arguments =
                  [ "prefix=${cfg.installDir}"
                  , "PREFIX=${cfg.installDir}"
                  , "install"
                  ]
                , environment = Some (buildEnv cfg)
                }
            )
        ]

in  { showVersion
    , makeGnuLibrary
    , makeGnuExe
    , defaultPackage
    , unbounded
    , lowerBound
    , upperBound
    , makeExe
    , printArch
    , printManufacturer
    , printOS
    , printTargetTriple
    , call
    , mkExe
    , mkExes
    , createDir
    , mkHost
    , defaultConfigure
    , defaultBuild
    , defaultInstall
    , cmakeConfigure
    , cmakeConfigureGeneral
    , cmakeConfigureWithFlags
    , cmakeBuild
    , cmakeInstall
    , cmakePackage
    , autogenConfigure
    , defaultCall
    , defaultEnv
    , maybeAppend
    , mkCFlags
    , mkLDFlags
    , mkLDFlagsGeneral
    , mkLDPath
    , mkLDRunPath
    , mkStaPath
    , libPath
    , mkPyPath
    , mkPy3Path
    , mkIncludePath
    , isUnix
    , defaultPath
    , simplePackage
    , symlinkBinary
    , symlinkManpage
    , symlink
    , symlinkBinaries
    , symlinkManpages
    , installWithBinaries
    , installWithManpages
    , configureMkExes
    , generalConfigure
    , configureWithFlags
    , configureMkExesExtraFlags
    , writeFile
    , cmakeInstallWithBinaries
    , copyFile
    , mkPathVar
    , mkPkgConfigVar
    , fullVersion
    , mesonConfigure
    , mesonConfigureGeneral
    , mesonEnv
    , mesonConfigureWithFlags
    , ninjaBuild
    , ninjaInstall
    , ninjaInstallWithPkgConfig
    , ninjaPackage
    , doNothing
    , perlConfigure
    , copyFiles
    , mkPerlLib
    , mesonMoves
    , python3Build
    , python3Install
    , python3Package
    , mkLDPreload
    , configureLinkExtraLibs
    , mkXdgDataDirs
    , buildWith
    , installWith
    , mkCCVar
    , squishVersion
    , osCfg
    , archCfg
    , mkCCArg
    , mkFRCArg
    , mesonCfgFile
    , python2Package
    , configEnv
    , configSome
    , preloadEnv
    , preloadCfg
    , printEnvVar
    , mkPyWrapper
    , mkPy3Wrapper
    , mkPy2Wrapper
    , installWithPyWrappers
    , installWithPy3Wrappers
    , cmakeConfigureNinja
    , mkLDPathWrapper
    , mkLDPathWrappers
    , installWithWrappers
    , cmakeEnv
    , cmakeSome
    , underscoreVersion
    , isX64
    , configWithEnv
    , buildEnv
    , patch
    , mkAclocalPath
    , configureWithPatches
    , configureWithPatch
    , installPrefix
    , unixPath
    , generalBuild
    , defaultCpus
    , singleThreaded
    , libSuffix
    }
