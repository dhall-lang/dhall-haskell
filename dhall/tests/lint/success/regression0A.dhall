    let replicate = https://prelude.dhall-lang.org/List/replicate

in  let Config = { name : Text, age : Natural }

in  let Configs = List Config

in  replicate 10 Text "!"
