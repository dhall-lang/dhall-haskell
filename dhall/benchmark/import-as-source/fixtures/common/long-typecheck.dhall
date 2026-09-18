-- typechecking this file involves long-eval which takes 1 second per factor. long-eval 0 is instantaneous, long-eval 1 is about 1 second.
let long-eval =
        missing
          sha256:f07ef17ada6a2778286ef56ab5619c7f98a22c2066c99db4fa92af5be2df30f9
      ? ./long-eval

let _ = assert : long-eval 1 === 0

in  True
