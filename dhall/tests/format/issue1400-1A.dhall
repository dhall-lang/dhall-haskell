{ conversation =
    [ { author = "robert", content = xs }
    , { author = "robert", content = xs }
    , { author =
          "bob"
      , content =
          [ text
              ''
              any line going past the 80 characters boundary     (e.g. this one is 82)
              ''
          ]
      }
    ]
}
