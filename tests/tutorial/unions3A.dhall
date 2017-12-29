    let MyType = constructors < Empty : {} | Person : { name : Text, age : Natural } >
in  [   MyType.Empty {=}
    ,   MyType.Person { name = "John", age = +23 }
    ,   MyType.Person { name = "Amy" , age = +25 }
    ]
