
namespace FSql

module Parser3 =

  type State = { input: seq<char>; position: int }

  type Error = Error of string

  type Parser<'a, 'b> = Parser of (State 
                                    -> ('a -> State -> 'b)  // OK
                                    -> (Error -> 'b)        // Error 
                                    -> 'b) 

  let parse (Parser p) state ok error = p state ok error

  let pMap f p = Parser <| fun s ok error -> parse p s (ok |> f) error

  let pReturn a = Parser <| fun s ok _ -> ok a s

  let pBind m k = Parser <| fun s ok error ->
                              let cok x s = parse (k x) s ok error
                              parse m s cok error

  let pFail msg = Parser <| fun s _  error -> error msg
