-- Eight distinct hash-protected children, each with a medium normalize burden
-- (~60ms). Under `as Source`, preserve pays Code-mode hash-check + Source
-- expand once per child, amplifying remaining hashed-child duplication.
let c1 =
      ./slow/multi/1.dhall
        sha256:55a2bea04a8136523ac4d270c5105b01cf65b07d6d0281c9a80e54dcd930ecb7

let c2 =
      ./slow/multi/2.dhall
        sha256:8e2b70786e933afdbc52c7585e2a046f785e8a9787e82978e09ee4f79a0f4494

let c3 =
      ./slow/multi/3.dhall
        sha256:a50130ebca22e1612b7aad6fefcd4203cb1e97d57263c524db6960049caff7fc

let c4 =
      ./slow/multi/4.dhall
        sha256:365ec5a97bdde16f48731ae97eb4e839f6f4f47aad64249157731ffdc22bab07

let c5 =
      ./slow/multi/5.dhall
        sha256:22fbee29672deb92071edd471680a9c1874854b6e7e39b75aac6ca4e055bed1c

let c6 =
      ./slow/multi/6.dhall
        sha256:b9023323ec95dced28fcb85cb1b8fc182028a2fb0f6f0c7801b2497f36aefb25

let c7 =
      ./slow/multi/7.dhall
        sha256:cb29741a0938426d3f4f2aa03548ae4360a790423d73cb98fc133dd7896ed61e

let c8 =
      ./slow/multi/8.dhall
        sha256:fe32aae75ace3ff0d204c84c9e31afbd294805cbf297807df3b60f84712499af

in  { result = [ c1, c2, c3, c4, c5, c6, c7, c8 ] }
