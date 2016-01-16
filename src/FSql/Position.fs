namespace FSql

module Position = 
  type Position = 
    { name : string option
      line : int
      column : int
      absolute : int }
  
  let initialPos name = 
    { name = name
      line = 1
      column = 1
      absolute = 0 }
  
  let updatePosChar pos ch = 
    match ch with
    | '\r' -> pos
    | '\n' -> 
      { pos with line = pos.line + 1
                 column = 1
                 absolute = pos.absolute + 1 }
    | _ -> { pos with column = pos.column + 1
                      absolute = pos.absolute + 1 }
  
  let updatePosString pos (str : seq<char>) = Seq.fold (updatePosChar) pos str
