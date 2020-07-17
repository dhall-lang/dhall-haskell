let schemas = ./tests/schemas/personSchema.dhall

in  schemas.Person::{
    , addr = schemas.Address::{ country = "Switzerland" }
    , name = "Alice"
    }
