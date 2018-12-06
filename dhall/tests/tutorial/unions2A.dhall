    let Empty  = < Empty = {=} | Person : { name : Text, age : Natural } >
in  let Person =
        λ(p : { name : Text, age : Natural }) → < Person = p | Empty : {} >
in  [   Empty
    ,   Person { name = "John", age = 23 }
    ,   Person { name = "Amy" , age = 25 }
    ,   Empty
    ]
