
namespace FSql

module Parser =
  open Position
  open Error

  type State<'a> = { input: seq<'a>; position: Position }

  type Consumption = Consumed | Virgin

  let unexpectedErr msg = newErrorMessage (Unexpected msg)

  let eoi = "end of input"

  type Result<'a> = 
                | Ok of 'a
                | Error of ParseError

  type Reply<'a,'b> = { state : State<'a>; consumption: Consumption; result: Result<'b> }

  type Hints = { hints: string list list }

  let toHints parserError = 
    let msgs = List.filter (fromEnum >> ((=) 1)) parserError.messages
    let hints = if List.isEmpty msgs then List.empty else [List.map messageString msgs]
    {hints = hints}

  type Parser<'a, 'b, 'c> = Parser of (State<'a>
                                    -> ('b -> State<'a> -> 'c)  // Ok
                                    -> (ParseError -> 'c)     // Error 
                                    -> 'c) 
  let inline uncons s =
    let head = Seq.tryHead s
    match head with
    | None -> None
    | Some i -> Some (i, Seq.tail s)

  let showToken x = sprintf "%A" x

  let unParser (Parser p) state ok error = p state ok error

  let inline pReturn a = Parser <| fun s ok _ -> ok a s

  let pPure = pReturn

  let initialState name s = { input = s; position = (initialPos name)  }

  let runParsec' p s =
    let cok a s' = { state = s' ; consumption = Consumed; result = Ok a } 
    let cerr msg = { state = s  ; consumption = Consumed; result = Error msg } 
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

  let inline pFail msg = Parser <| fun s _ error -> 
    error <| newErrorMessage (Message msg) (s.position)

  let inline pFailure msgs = Parser <| fun s _ error ->
    error <| newErrorMessages msgs s.position

  let inline pZero () = Parser <| fun s _ error ->
    error <| newErrorUnknown s.position

  let inline pEof () = 
    Parser <| fun s ok error ->
      match uncons s.input with
      | None -> ok () s
      | Some (x,_) -> error <| unexpectedErr (showToken x) s.position

  let inline pToken nextpos test = 
    Parser <| fun s ok error ->
      match uncons s.input with
      | None -> error <| unexpectedErr "End Of Input" s.position
      | Some (c,cs) -> 
        match test c with 
        | Choice1Of2 err -> error <| addErrorMessages err (newErrorUnknown s.position)
        | Choice2Of2 a -> 
          let newpos = nextpos s.position c
          let newstate = { input = cs ; position = newpos }
          ok a newstate

  let inline pTokens nextpos test tts =
    match uncons tts with
    | None -> Parser <| fun s ok _ -> ok [] s
    | Some (_, _) -> Parser <| fun s ok error -> 
      let errExpect x = 
        setErrorMessage (showToken tts |> Expected) (newErrorMessage (Unexpected x) s.position)
      let rec walk ts is rs =
        match uncons ts with
        | None -> 
          let pos' = nextpos s.position tts
          let s' =  { input = rs ; position = pos' }
          ok (List.rev is) s'
        | Some (t, ts) ->
          let errorCont = if Seq.isEmpty is then error else error
          let what  = if Seq.isEmpty is then eoi else showToken <| List.rev is
          match uncons rs with
          | None -> (errExpect >> errorCont) <| what
          | Some (c, cs) ->
              if test t c then
                walk ts (c::is) cs
              else
                (showToken >> errExpect >> errorCont) <| List.rev (c::is)
      walk tts [] s.input

  let satisfy f = 
    let testChar (x: char) =
      if f x then Choice2Of2 x
      else (Choice1Of2 << List.singleton << Unexpected << showToken) <| x
    pToken updatePosChar testChar
