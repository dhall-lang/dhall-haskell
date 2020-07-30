let relatives =
    [ ./IndexesExample.dhall
    , ./MarkdownExample.dhall
    , ./NoDoc.dhall
    ]

let moreRelatives =
    [ ./OrdinaryAnnotation.dhall ,    ./TwoAnnotations.dhall ]

in relatives # moreRelatives
