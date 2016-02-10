
#time 
#load "Position.fs"
#load "Error.fs"
#load "Primitive.fs"

open FSql.Primitive

printfn "Basic Tests"

let b1 = runParsec' (char' 'a') (initialState None "abc")

printfn "b1 - %A" b1

let b2 = runParsec' (char' 'b') (initialState None "abc")

printfn "b2 - %A" b2
printfn "NextStep Tests"

let char2 a b = parser { let! c1 = char' a
                         let! c2 = char' b
                         return (c1, c2) }
let res1 = runParsec' (char2 'a' 'b') (initialState None "ab")

printfn "res1 - %A" res1

let res2 = runParsec' (char2 'a' 'b') (initialState None "ac")

printfn "res2 - %A" res2

let char2eof a b = parser { let! c1 = char2 a b
                            let! c2 = pEof()
                            return c1 }
let res3 = runParsec' (char2eof 'a' 'b') (initialState None "ab")

printfn "res3 - %A" res3

let res4 = runParsec' (char2eof 'a' 'b') (initialState None "ab ")

printfn "res4 - %A" res4

let res5 = runParsec' (string' "ab") (initialState None "ab")

printfn "res5 - %A" res5

let res6 = runParsec' (string' "ab") (initialState None "ac")

printfn "res6 - %A" res6
