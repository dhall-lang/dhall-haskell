  λ(isActive : Bool)
→   { barLeftEnd =
        [ "┨" ] : Optional Text
    , barRightEnd =
        [ "┠" ] : Optional Text
    , separator =
        [ "┃" ] : Optional Text
    , alignment =
          < ToTheLeft = {=} | ToTheRight : {} | Centered : {} >
        : ./Alignment.dhall
    , barWidth =
        [] : Optional Natural
    , barSegments =
        [ "index", "command", "path", "title" ]
    }
  : ./Bar.dhall
