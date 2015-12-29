
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

  let hEmpty = { hints = List.empty }

  let toHints parserError = 
    let msgs = List.filter (fromEnum >> ((=) 1)) parserError.messages
    let hints = if List.isEmpty msgs then List.empty else [List.map messageString msgs]
    {hints = hints}

  //let withHints hints error = 
  //  addErrorMessages (Expected <| String.concat String.empty hints.hints)

  //let accHints = 

  let refreshLastHint hs l = 
    match (hs, l) with
    | ({ hints = _::cs }, "") -> { hints = cs }
    | ({ hints = _::cs }, l') -> { hints = [l']::cs }
    | (hEmpty, _) -> hEmpty

  type Parser<'a, 'b, 'c> = Parser of (State<'a>
                                    -> ('b -> State<'a> -> Hints -> 'c)  // Ok
                                    -> (ParseError -> 'c)     // Error 
                                    -> 'c) 
  let inline uncons s =
    let head = Seq.tryHead s
    match head with
    | None -> None
    | Some i -> Some (i, Seq.tail s)

  let showToken x = sprintf "%A" x

  let unParser (Parser p) state ok error = p state ok error

  let inline pReturn a = Parser <| fun s ok _ -> ok a s hEmpty

  let pPure = pReturn

  let initialState name s = { input = s; position = (initialPos name)  }

  let runParsec' p s =
    let cok a s' _ = { state = s' ; consumption = Consumed; result = Ok a } 
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
                              let cok x s h = unParser (k x) s ok error
                              unParser m s cok error

  let inline pFail msg = Parser <| fun s _ error -> 
    error <| newErrorMessage (Message msg) (s.position)

  let inline pFailure msgs = Parser <| fun s _ error ->
    error <| newErrorMessages msgs s.position

  let inline pZero () = Parser <| fun s _ error ->
    error <| newErrorUnknown s.position

  let inline pLabel l p = Parser <| fun s cok cerr ->
    let l' = if Seq.isEmpty l then l else "rest of " + l
    let cok' x s' hs = cok x s' <| refreshLastHint hs l
    unParser p s cok' cerr
    

  let inline pEof () = 
    let parser = Parser <| fun s ok error ->
      match uncons s.input with
      | None -> ok () s hEmpty
      | Some (x,_) -> error <| unexpectedErr (showToken x) s.position
    pLabel eoi parser

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
          ok a newstate hEmpty

  let inline pTokens nextpos test tts =
    match uncons tts with
    | None -> Parser <| fun s ok _ -> ok [] s hEmpty
    | Some (_, _) -> Parser <| fun s ok error -> 
      let errExpect x = 
        setErrorMessage (showToken tts |> Expected) (newErrorMessage (Unexpected x) s.position)
      let rec walk ts is rs =
        match uncons ts with
        | None -> 
          let pos' = nextpos s.position tts
          let s' =  { input = rs ; position = pos' }
          ok (List.rev is) s' hEmpty
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

  let char' c =  satisfy ((=)c)

  type ParserBuilder () =
    member x.Bind(func, comp) = pBind func comp
    member x.Return(value) = pReturn value
    member x.ReturnFrom(value) = value
    member x.Zero() = pZero ()

  let parser = new ParserBuilder ()

  let char2 a b = parser {
                    let! c1 = char' a
                    let! c2 = char' b
                    return (c1, c2)
                  }
