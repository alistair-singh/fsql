namespace FSql

module Parser2 =

  open FSql.Cursor
  open FSql.Position

  type Message = 
    | Unexpected of string
    | Expected of string
    | Message of string

  type ParseError =
    { Position: Position
      Messages: Message list }

  type State = 
    { Cursor: char Cursor
      Position: Position }

  type 'a Consumed = 
    | Consumed of 'a
    | Empty of 'a

  type 'a Reply = 
    | Ok of ('a * State)
    | Error of ParseError

  type Hints = Hints of string list list

