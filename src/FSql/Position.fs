namespace FSql

module Position =

  type Position = { name : string option; line: int; column: int; }

  let initialPos name = 
     { name = name; line = 1; column = 1 }

  let updatePosChar pos ch =
    match ch with
    | '\n' -> { pos with line = pos.line+1; column = 1 }
    | _    -> { pos with column = pos.column + 1 }

