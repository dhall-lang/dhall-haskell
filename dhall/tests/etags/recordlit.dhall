{- This is an example Dhall configuration file

   Can you spot the mistake?

   Fix the typo, then move onto the "Definitions" example
-}

{ home       = "/home/bill"
, privateKey = "/home/bill/.ssh/id_ed25519"
, publicKey  = "/home/blil/.ssh/id_ed25519.pub"
, name = { firstName = "Bill"
         , secondName = "Bar"
         }
}
