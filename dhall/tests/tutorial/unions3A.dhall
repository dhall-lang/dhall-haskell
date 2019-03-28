let MyType = < Empty | Person : { name : Text, age : Natural } >

in  [   MyType.Empty  -- Note the absence of any argument to `Empty`
    ,   MyType.Person { name = "John", age = 23 }
    ,   MyType.Person { name = "Amy" , age = 25 }
    ]
