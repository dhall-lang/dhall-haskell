let imageRecordType =
      { name : Text, registry : Text, sha256 : Text, version : Text }

let baseType = < asText : Text | asRecord : imageRecordType >

let customization = ../customizations/image.dhall

let overlayMergeHelper
    : imageRecordType →
      customization.Type →
      Optional { version : Text, sha256 : Text } →
        baseType
    = λ(base : imageRecordType) →
      λ(overlay : customization.Type) →
      λ(overlayVs : Optional { version : Text, sha256 : Text }) →
        let overrideVersion =
              merge
                { Some =
                    λ(x : { version : Text, sha256 : Text }) → Some x.version
                , None = None Text
                }
                overlayVs

        let overrideSha256 =
              merge
                { Some =
                    λ(x : { version : Text, sha256 : Text }) → Some x.sha256
                , None = None Text
                }
                overlayVs

        let overlayedRegistry =
              merge
                { Some = λ(x : Text) → x, None = base.registry }
                overlay.registry

        let prefixedName =
              merge
                { Some = λ(x : Text) → x ++ base.name, None = base.name }
                overlay.prefix

        let prefixedAndSuffixName =
              merge
                { Some = λ(x : Text) → prefixedName ++ x, None = prefixedName }
                overlay.suffix

        let finalVersion =
              merge
                { Some = λ(x : Text) → x, None = base.version }
                overrideVersion

        let finalSha356 =
              merge
                { Some = λ(x : Text) → x, None = base.sha256 }
                overrideSha256

        let p1 = prefixedAndSuffixName ++ ":" ++ finalVersion

        let p2 =
              if    overlay.omitRegistry
              then  p1
              else  overlayedRegistry ++ "/" ++ p1

        let p3 =
              if overlay.omitSha256 then p2 else p2 ++ "@sha256:" ++ finalSha356

        in  baseType.asText p3

let overlayMerge
    : baseType →
      customization.Type →
      Optional { version : Text, sha256 : Text } →
        baseType
    = λ(base : baseType) →
      λ(overlay : customization.Type) →
      λ(overlayVs : Optional { version : Text, sha256 : Text }) →
        merge
          { asText = λ(x : Text) → baseType.asText x
          , asRecord =
              λ(x : imageRecordType) → overlayMergeHelper x overlay overlayVs
          }
          base

let tests =
      { t1 =
            assert
          :   overlayMerge
                ( baseType.asRecord
                    { name = "foo"
                    , registry = "docker.io"
                    , sha256 =
                        "776606b680d7ce4a5d37451831ef2414ab10414b5e945ed5f50fe768f898d23f"
                    , version = "3.19.2"
                    }
                )
                customization::{=}
                (None { version : Text, sha256 : Text })
            ≡ baseType.asText
                "docker.io/foo:3.19.2@sha256:776606b680d7ce4a5d37451831ef2414ab10414b5e945ed5f50fe768f898d23f"
      , t2 =
            assert
          :   overlayMerge
                ( baseType.asRecord
                    { name = "foo"
                    , registry = "docker.io"
                    , sha256 =
                        "776606b680d7ce4a5d37451831ef2414ab10414b5e945ed5f50fe768f898d23f"
                    , version = "3.19.2"
                    }
                )
                customization::{ registry = Some "google.io" }
                (None { version : Text, sha256 : Text })
            ≡ baseType.asText
                "google.io/foo:3.19.2@sha256:776606b680d7ce4a5d37451831ef2414ab10414b5e945ed5f50fe768f898d23f"
      , t3 =
            assert
          :   overlayMerge
                ( baseType.asRecord
                    { name = "foo"
                    , registry = "docker.io"
                    , sha256 =
                        "776606b680d7ce4a5d37451831ef2414ab10414b5e945ed5f50fe768f898d23f"
                    , version = "3.19.2"
                    }
                )
                customization::{
                , registry = Some "google.io"
                , prefix = Some "pre"
                , suffix = Some "ix"
                }
                (None { version : Text, sha256 : Text })
            ≡ baseType.asText
                "google.io/prefooix:3.19.2@sha256:776606b680d7ce4a5d37451831ef2414ab10414b5e945ed5f50fe768f898d23f"
      , t4 =
            assert
          :   overlayMerge
                ( baseType.asRecord
                    { name = "foo"
                    , registry = "docker.io"
                    , sha256 =
                        "776606b680d7ce4a5d37451831ef2414ab10414b5e945ed5f50fe768f898d23f"
                    , version = "3.19.2"
                    }
                )
                customization::{
                , registry = Some "google.io"
                , prefix = Some "pre"
                , suffix = Some "ix"
                }
                ( Some
                    { version = "insiders"
                    , sha256 =
                        "51b827ff2bf3f9df740cd8538840ef7fd7cb245c7b93063ef829f67ca71ea23e"
                    }
                )
            ≡ baseType.asText
                "google.io/prefooix:insiders@sha256:51b827ff2bf3f9df740cd8538840ef7fd7cb245c7b93063ef829f67ca71ea23e"
      , t5 =
            assert
          :   overlayMerge
                ( baseType.asRecord
                    { name = "foo"
                    , registry = "docker.io"
                    , sha256 =
                        "776606b680d7ce4a5d37451831ef2414ab10414b5e945ed5f50fe768f898d23f"
                    , version = "3.19.2"
                    }
                )
                customization::{
                , registry = Some "google.io"
                , prefix = Some "pre"
                , suffix = Some "ix"
                , omitRegistry = True
                }
                (None { version : Text, sha256 : Text })
            ≡ baseType.asText
                "prefooix:3.19.2@sha256:776606b680d7ce4a5d37451831ef2414ab10414b5e945ed5f50fe768f898d23f"
      , t6 =
            assert
          :   overlayMerge
                ( baseType.asRecord
                    { name = "foo"
                    , registry = "docker.io"
                    , sha256 =
                        "776606b680d7ce4a5d37451831ef2414ab10414b5e945ed5f50fe768f898d23f"
                    , version = "3.19.2"
                    }
                )
                customization::{
                , registry = Some "google.io"
                , prefix = Some "pre"
                , suffix = Some "ix"
                , omitSha256 = True
                }
                (None { version : Text, sha256 : Text })
            ≡ baseType.asText "google.io/prefooix:3.19.2"
      }

in  overlayMerge
