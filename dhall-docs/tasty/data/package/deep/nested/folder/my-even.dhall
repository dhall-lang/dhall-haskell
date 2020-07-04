{-
Check if a natural is even or not returning an union data-type
-}
let my-even
    : Natural → < even | odd >
    = λ(n : Natural) →
        if Natural/even n then < even | odd>.even else < even | odd >.odd

let example0 = assert : my-even 1 === <even | odd>.odd
let example0 = assert : my-even 2 === <even | odd>.even

in my-even
