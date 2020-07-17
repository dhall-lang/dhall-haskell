let Address =
      { Type = { country : Text, city : Optional Text }
      , default.city = None Text
      }

let Person =
      { Type = { name : Text, addr : Address.Type, alive : Bool }
      , default.alive = True
      }

in  { Person, Address }
