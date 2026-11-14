{ NonEmpty.Type = ./dhall-lang/Prelude/NonEmpty/Type.dhall
, List =
    { drop = ./dhall-lang/Prelude/List/drop.dhall
    , head = ./dhall-lang/Prelude/List/head.dhall
    , map = ./dhall-lang/Prelude/List/map.dhall
    , concatMap = ./dhall-lang/Prelude/List/concatMap.dhall
    , filterMap = ./dhall-lang/Prelude/List/filterMap.dhall
    , zip = ./dhall-lang/Prelude/List/zip.dhall
    , foldLeft = ./dhall-lang/Prelude/List/foldLeft.dhall
    , length = ./dhall-lang/Prelude/List/length.dhall
    }
, Function.compose = ./dhall-lang/Prelude/Function/compose.dhall
, Optional.map = ./dhall-lang/Prelude/Optional/map.dhall
, Text =
    { concatSep = ./dhall-lang/Prelude/Text/concatSep.dhall
    , concatMap = ./dhall-lang/Prelude/Text/concatMap.dhall
    }
, Natural =
    { show = ./dhall-lang/Prelude/Natural/show.dhall
    , lessThan = ./dhall-lang/Prelude/Natural/lessThan.dhall
    }
}
