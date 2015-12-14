
namespace FSql

module Parser3 =

  type Position = { line: int; column: int; absolute: int }

  type State<'a> = { input: seq<'a>; position: Position }

  type Consumption = Consumed | Virgin

  type Message = Unexpected of string
               | Expected of string
               | Message of string

  let messageString = function
    | Unexpected s -> s
    | Expected s -> s
    | Message s -> s

  let fromEnum = function
    | Unexpected _ -> 0
    | Expected _ -> 1
    | Message _ -> 2

  type ParseError = { position: Position ; messages : Message list }

  let addErrorMessage m err = 
    let pre = List.filter ((<) m) (err.messages)
    let post = List.filter ((>) m) (err.messages)
    let msgs = pre @ [m] @ post
    { err with messages = msgs }

  let addErrorMessages = List.foldBack addErrorMessage 

  let newErrorUnknown pos = { position = pos; messages = List.empty }

  let newErrorMessages msgs pos = addErrorMessages msgs <| newErrorUnknown pos

  let newErrorMessage msg = newErrorMessages [msg]

  let badMessage msgs = Seq.isEmpty msgs

  //let setErrorMessage msg error =
    //let xs = filter (fromEnum >> ((!=) fromEnum msg) error.messages
    //let err = { error with messages = xs }
    //if badMessage msg then err else addErrorMessages msg err

  let unexpectedErr msg = newErrorMessage (Unexpected msg)

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

  let pReturn a = Parser <| fun s ok _ -> ok a s

  let pPure = pReturn

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
    let message = Message msg 
    error <| newErrorMessage message (s.position)

  let inline pZero () = pFail <| "Zero Error"

  let inline pEos () = 
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
    //let errExpect x = setErrorMessage (showToken tts |> Expected)
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
          | None -> error <| newErrorMessage (Expected "no clue") s.position
          | Some (c, cs) ->
              if test t c then
                walk ts (c::is) cs
              else
                error <| newErrorMessage (Expected "no clue 2") s.position
      walk tts [] s.input

