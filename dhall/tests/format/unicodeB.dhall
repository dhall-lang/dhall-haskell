  λ(isActive : Bool)
→   { barLeftEnd =
        Some "┨"
    , barRightEnd =
        Some "┠"
    , separator =
        Some "┃"
    , alignment =
          < ToTheLeft = {=} | ToTheRight : {} | Centered : {} >
        : ./Alignment.dhall
    , barWidth =
        None Natural
    , barSegments =
        [ "index", "command", "path", "title" ]
    }
  : ./Bar.dhall