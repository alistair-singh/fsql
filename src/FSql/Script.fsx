
#time 

#load "Position.fs"
#load "Cursor.fs"
#load "Parser.fs"
#load "Parser2.fs"

open System.IO

open FSql

let fileCursor path =
  let str = File.ReadAllText (path)
  let data = Array.ofSeq str
  (Position.startFile path, Cursor.Valid (0, data))

let stringCursor str =
  let data = Array.ofSeq str
  (Position.start, Cursor.Valid (0, data))

let cursor = fileCursor "Position.fs"

let pos' = 
  function 
  | Parser.Item (i, p) -> p
  | Parser.Error (i, p) -> p
 
let tuple4 =
  Parser.parser {
    let! i1 = Parser.item
    let! i2 = Parser.item
    let! i3 = Parser.item
    let! i4 = Parser.item
    return! Parser.Parser(fun cs -> [Parser.Item ((i1, i2, i3, i4), pos' i1), cs])
  }

let res = Parser.parse (tuple4) (stringCursor "hel")
printfn "%A" res


