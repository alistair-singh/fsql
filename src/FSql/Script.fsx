
#time 

#load "Position.fs"
#load "Error.fs"
#load "Parser.fs"

open FSql.Parser

printfn "Basic Tests"

let b1 = runParsec' (char' 'a') (initialState None "abc")
printfn "%A" b1

let b2 = runParsec' (char' 'b') (initialState None "abc")
printfn "%A" b2

printfn "NextStep Tests"

let char2 a b = parser {
                  let! c1 = char' a
                  let! c2 = char' b
                  return (c1, c2)
                }

let res1 = runParsec' (char2 'a' 'b') (initialState None "ab")
printfn "%A" res1

let res2 = runParsec' (char2 'a' 'b') (initialState None "ac")
printfn "%A" res2

