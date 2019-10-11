let User = { name : Text, home : Text }

let mkUser =
        λ(_isAdmin : Bool)
      →       if _isAdmin

        then  { name = "admin", home = "/home/admin" }

        else  { name = "default", home = "/home/user" }

in  mkUser True : User
