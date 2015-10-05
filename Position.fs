module Position

type Pos = int * int * string option

let startFile path = (0, 0, Some path) 
let start = (0, 0, None) 

let advance ch =
  function
  | (line, column, path) ->
    match ch with
    | '\r' -> (line, column, path)
    | '\n' -> (line + 1, 0, path)
    | _ -> (line, column + 1, path)

let toString =
  function
  | (line, column, None) -> sprintf "(%i, %i)" (line + 1) (column + 1)
  | (line, column, Some path) -> sprintf "%s(%i %i)" path (line + 1) (column + 1)

