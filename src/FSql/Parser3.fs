
namespace FSql

module Parser3 =

  type State = { input: seq<char>; position: int }

  type Result<'a> = 
                | Error of string
                | Ok of 'a

  type Reply<'a> = { state : State; result: Result<'a> }

  type Parser<'a, 'b> = Parser of (State 
                                    -> ('a -> State -> 'b)  // Ok
                                    -> (string -> 'b)     // Error 
                                    -> 'b) 
  let inline uncons s =
    let head = Seq.tryHead s
    match head with
    | None -> None
    | Some i -> Some (i, Seq.tail s)

  let unParser (Parser p) state ok error = p state ok error

  let pReturn a = Parser <| fun s ok _ -> ok a s

  let pPure = pReturn

  let runParsec' p s =
    let cok a s' = { state = s' ; result = Ok a } 
    let cerr msg = { state = s  ; result = Error msg } 
    unParser p s cok cerr

  let runParser' p s =
    let reply = runParsec' p s
    let { state = s' ; result = r } = reply
    match r with
    | Error msg -> (s', Choice1Of2 msg)
    | Ok a -> (s', Choice2Of2 a)

  let runParser p s =
    runParser' p s |> snd

  let inline pMap f p = Parser <| fun s ok error -> unParser p s (ok |> f) error

  let inline pBind m k = Parser <| fun s ok error ->
                              let cok x s = unParser (k x) s ok error
                              unParser m s cok error

  let inline pFail msg = Parser <| fun _ _ error -> error msg

  let inline pZero () = pFail "Zero Error"

  let inline pEos () = 
    Parser <| fun s ok error ->
      match uncons s.input with
      | None -> ok () s
      | Some (x,xs) -> sprintf "Unexpected %A" x |> error

  let inline pToken nextpos test = 
    Parser <| fun s ok error ->
      match uncons s.input with
      | None -> error "End Of Input"
      | Some (c,cs) -> 
        match test c with 
        | Choice1Of2 err -> error err
        | Choice2Of2 a -> 
          let newpos = nextpos s.position c
          let newstate = { input = cs ; position = newpos }
          ok a newstate

  let inline pTokens nextpos test tts =
    match uncons tts with
    | None -> Parser <| fun s ok _ -> ok [] s
    | Some (_, _) -> Parser <| fun s ok error -> 
      let rec walk ts is rs =
        match uncons ts with
        | None -> 
          let pos' = nextpos s.position tts
          let s' =  { input = rs ; position = pos' }
          ok (List.rev is) s'
        | Some (t, ts) ->
          match uncons rs with
          | None -> error "no clue"
          | Some (c, cs) ->
              if test t c then
                walk ts (c::is) cs
              else
                error "no clue 2"
      walk tts [] s.input

