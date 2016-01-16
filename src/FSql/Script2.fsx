type Message = Unexpected of string
             | Expected of string
             | Message of string

type Position = { name : string option; line: int; column: int; }

type ParseError = { position: Position ; messages : Message list }

type State<'a> = { input: seq<'a>; position: Position }

type Consumption = Consumed | Virgin

type Result<'a> = 
              | Ok of 'a
              | Error of ParseError

type Reply<'a,'b> = { state : State<'a>; consumption: Consumption; result: Result<'b> }

type Cursor<'a> = { index: int; values : 'a array }

let mk () = { index = 0
              values = Array.ofSeq (Seq.replicate 100000000 'a')}
  //{ state = 
  //    { input = Seq.tail "abc"
  //      position = 
  //        { name = None
  //          line = 1
  //          column = 2 } }
  //  result = Ok 'a'
  //  consumption = Consumed }

let expected = mk ()
let expected2 = mk ()

if expected = expected2 
  then printfn "matched" |> ignore
  else printfn "unmatched" |> ignore

