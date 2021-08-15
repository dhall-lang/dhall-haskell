let multiLineText = ''
Line1
Line2
Line3
''
let answer = 42
let dhall = "Dhall"

in

[ { someText = "Hello World!" }
, { someText = "Hello, World!" }
, { someText = multiLineText }
, { someText = "The answer to life, the universe, and everything: ${Natural/show answer}" }
, { someText = "Hello ${dhall}!" }
]
