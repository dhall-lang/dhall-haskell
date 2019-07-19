let Example = < Left : { foo : Natural } | Right : { bar : Bool } >

let Nesting = < Inline : {} | Nested : Text >

in  { field    = "name"
    , nesting  = Nesting.Nested "value"
    , contents = Example.Left { foo = 2 }
    }
