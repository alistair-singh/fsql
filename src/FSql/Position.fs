namespace FSql

module Position = 
  type Position = 
    { name : string option
      line : int
      column : int
      absolute : int }
  
  //TODO: Rename to initialPosition
  let initialPos name = 
    { name = name
      line = 1
      column = 1
      absolute = 0 }
  
  //TODO: Rename to updatePositionChar
  let updatePosChar pos ch = 
    match ch with
    | '\r' -> 
      { pos with absolute = pos.absolute + 1 }
    | '\n' -> 
      { pos with line = pos.line + 1
                 column = 1
                 absolute = pos.absolute + 1 }
    | _ -> { pos with column = pos.column + 1
                      absolute = pos.absolute + 1 }
  
  //TODO: Rename to updatePositionString
  let updatePosString pos (str : seq<char>) = Seq.fold (updatePosChar) pos str

