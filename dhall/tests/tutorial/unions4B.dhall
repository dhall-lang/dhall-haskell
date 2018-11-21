{ Empty  =
    λ(Empty : {}) → < Empty = Empty | Person : { age : Natural, name : Text } >
, Person =
      λ(Person : { age : Natural, name : Text })
    → < Person = Person | Empty : {} >
}
