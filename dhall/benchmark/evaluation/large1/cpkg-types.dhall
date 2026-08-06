let OS =
      < FreeBSD
      | OpenBSD
      | NetBSD
      | Solaris
      | Dragonfly
      | Linux
      | Darwin
      | Windows
      | Redox
      | Haiku
      | IOS
      | AIX
      | Hurd
      | Android
      | NoOs
      >

let Arch =
      < X64
      | AArch
      | Arm
      | RISCV64
      | PowerPC
      | PowerPC64
      | PowerPC64le
      | Sparc64
      | S390x
      | Alpha
      | M68k
      | Mips
      | MipsEl
      | Mips64
      | Mips64El
      | X86
      | SH4
      | HPPA
      | HPPA64
      | MipsIsa32r6El
      | MipsIsa32r6
      | MipsIsa64r6El
      | MipsIsa64r6
      >

let Manufacturer = < Unknown | Apple | IBM | PC >

let ABI = < GNU | GNUabi64 | GNUeabi | GNUeabihf | GNUspe | MinGw >

let TargetTriple =
      { arch : Arch
      , manufacturer : Optional Manufacturer
      , os : OS
      , abi : Optional ABI
      }

let BuildVars =
      { installDir : Text
      , currentDir : Text
      , targetTriple : Optional TargetTriple
      , isCross : Bool
      , includeDirs : List Text
      , preloadLibs : List Text
      , shareDirs : List Text
      , linkDirs : List Text
      , binDirs : List Text
      , buildOS : OS
      , buildArch : Arch
      , static : Bool
      , cpus : Natural
      }

let VersionBound =
      < Lower : { lower : List Natural }
      | Upper : { upper : List Natural }
      | LowerUpper : { lower : List Natural, upper : List Natural }
      | NoBound
      >

let Dep = { name : Text, bound : VersionBound }

let EnvVar = { var : Text, value : Text }

let Proc =
      { program : Text
      , arguments : List Text
      , environment : Optional (List EnvVar)
      , procDir : Optional Text
      }

let Command =
      < CreateDirectory : { dir : Text }
      | MakeExecutable : { file : Text }
      | Call : Proc
      | SymlinkBinary : { file : Text }
      | SymlinkManpage : { file : Text, section : Natural }
      | Symlink : { tgt : Text, linkName : Text }
      | Write : { file : Text, contents : Text }
      | CopyFile : { src : Text, dest : Text }
      | Patch : { patchContents : Text }
      >

in  { OS
    , BuildVars
    , VersionBound
    , Dep
    , Arch
    , Manufacturer
    , ABI
    , TargetTriple
    , Command
    , EnvVar
    , Proc
    }
