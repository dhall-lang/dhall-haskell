let compose
    : forall (a : Type) -> forall (b : Type) -> forall (c : Type) -> (a -> b) -> (b -> c) -> a -> c
    = \(a : Type) ->
      \(b : Type) ->
      \(c : Type) ->
      \(f : a -> b) ->
      \(g : b -> c) ->
      \(x : a) ->
        g (f x)

let composeList
    : forall (a : Type) -> List (a -> a) -> a -> a
    = \(a : Type) ->
      \(functions : List (a -> a)) ->
        List/fold (a -> a) functions (a -> a) (compose a a a) (\(a : a) -> a)

let concatMap
    : forall (a : Type) -> (a -> Text) -> List a -> Text
    = 
        \(a : Type) ->
        \(f : a -> Text) ->
        \(xs : List a) ->
            List/fold a xs Text (\(x : a) -> \(y : Text) -> f x ++ y) ""

let escapeText
    : Text -> Text
    = composeList
        Text
        [ Text/replace "\\" "\\\\"
        , Text/replace "\"" "\\\""
        , Text/replace "\n" ("\\n\\" ++ "\n" ++ "\\")
        ]
        
let Fragment = <A : Text | B : Natural>

let renderExp
    : List Fragment -> Text
    = \(fragments : List Fragment) ->
            "\""
        ++  concatMap
            Fragment
            ( \(fragment : Fragment) ->
                merge
                    { A = escapeText
                    , B =
                        \(b : Natural) ->
                        "\$" ++ Natural/show b
                    }
                    fragment
            )
            fragments
                    ++  "\""

let fn =
      \(fragments : List Fragment) ->
        Text/replace "." "!" (renderExp fragments)

in fn