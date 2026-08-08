let configuration =
    -- Note. I did this very quickly for the demo and I'm
    -- actually not sure whether or not we should bundle this at all.
    -- For right now, I'm going to only allow just these two
    -- simple overrides
      { Type = { Enabled : Bool, IPAddress : Optional Text }
      , default = { Enabled = False, IPAddress = None Text }
      }

in  configuration
