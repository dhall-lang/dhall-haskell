let makeUser = λ(user : Text) → let home = "/home/${user}" in { home = home }

in  { makeUser = makeUser, `make user` = makeUser }
