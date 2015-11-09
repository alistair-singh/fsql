namespace FSql

module Position =

  type Position = Position of line: int * column: int * path: string option

  let startFile path = Position (0, 0, Some path) 
  let start = Position (0, 0, None) 

  let advance ch =
    function
    | Position (line, column, path) ->
      match ch with
      | '\r' -> Position (line, column, path)
      | '\n' -> Position (line + 1, 0, path)
      | _ -> Position (line, column + 1, path)

  let toString =
    function
    | Position (line, column, None) -> sprintf "(%i, %i)" (line + 1) (column + 1)
    | Position (line, column, Some path) -> sprintf "%s(%i %i)" path (line + 1) (column + 1)
