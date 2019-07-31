  λ(isActive : Bool)
→   { barLeftEnd =
        Some "┨"
    , barRightEnd =
        Some "┠"
    , separator =
        Some "┃"
    , alignment =
        < ToTheLeft | ToTheRight | Centered >.ToTheLeft : ./Alignment.dhall
    , barWidth =
        None Natural
    , barSegments =
        [ "index", "command", "path", "title" ]
    }
  : ./Bar.dhall