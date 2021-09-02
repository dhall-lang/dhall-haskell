-- https://github.com/dhall-lang/dhall-haskell/issues/2273
let MyPackage =
      https://server.test/package.dhall using (./headers.dhall)
        sha256:03a6e298ff140d430cea8b387fad886ce9f5bee24622c7d1102115cc08ed9cf8

in  MyPackage
