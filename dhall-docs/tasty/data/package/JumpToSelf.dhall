{-|
Althtough Dhall forbids cyclic imports, `dhall-docs` can handle them properly
-}
let selfs =
    [ ./JumpToSelf.dhall
    , ../package/JumpToSelf.dhall ]
in selfs
