namespace FSql

module Primitive = 
  open Position
  open Error
  
  [<CustomEquality; NoComparison>]
  type State<'a when 'a : equality> = 
    { input : seq<'a>
      position : Position }
    override x.Equals(yobj) = 
      match yobj with
      | :? State<'a> as y -> 
        let compare seqx seqy =
          let lengthMatch = (Seq.length seqx) = (Seq.length seqy)
          let itemsMatch = Seq.forall2 (=) seqx seqy
          lengthMatch && itemsMatch
        (x.position = y.position) && (compare x.input y.input)
      | _ -> false
    override x.GetHashCode() =
      x.position.GetHashCode() //TODO: Make hashcode include input
  
  type Consumption = 
    | Consumed
    | Virgin
  
  let unexpectedErr msg = newErrorMessage (Unexpected msg)
  let eoi = "end of input"
  
  type Result<'a> = 
    | Ok of 'a
    | Error of ParseError
  
  type Reply<'a, 'b when 'a : equality> = 
    { state : State<'a>
      consumption : Consumption
      result : Result<'b> }
  
  type Hints = 
    { hints : string list list }
  
  let hEmpty = { hints = List.empty }
  
  let toHints parserError = 
    let msgs = List.filter (fromEnum >> ((=) 1)) parserError.messages
    
    let hints = 
      if List.isEmpty msgs then List.empty
      else [ List.map messageString msgs ]
    { hints = hints }
  
  let withHints hints c e state = 
    let isMessage = 
      function 
      | Message _ -> true
      | _ -> false
    if Seq.forall isMessage e.messages then c e state
    else 
      let errorMessages = Seq.concat hints.hints
                          |> Seq.map Expected
                          |> List.ofSeq
                          |> addErrorMessages
      (c << errorMessages) e state
  
  let accHints hs1 c x s hs2 = c x s { hints = (hs1.hints @ hs2.hints) }
  
  let refreshLastHint hs l = 
    match (hs, l) with
    | ({ hints = _ :: cs }, "") -> { hints = cs }
    | ({ hints = _ :: cs }, l') -> { hints = [ l' ] :: cs }
    | (hEmpty, _) -> hEmpty
  
  type Parser<'a, 'b, 'c when 'a : equality> = 
    | Parser of (State<'a> -> ('b -> State<'a> -> Hints -> 'c)  // consumed Ok
                           -> (ParseError -> State<'a> -> 'c)   // consumed error
                           -> ('b -> State<'a> -> Hints -> 'c)  // empty Ok
                           -> (ParseError -> State<'a> -> 'c)   // empty error
                           -> 'c)

  let inline uncons s = 
    let head = Seq.tryHead s
    match head with
    | None -> None
    | Some i -> Some(i, Seq.tail s)
  
  let showToken x = sprintf "%A" x
  let unParser (Parser p) state cok cerr eok eerr = p state cok cerr eok eerr
  let inline pReturn a = Parser <| fun s _ _ eok _ -> eok a s hEmpty
  let pPure = pReturn
  
  let initialState name s = 
    { input = s
      position = (initialPos name) }
  
  let runParsec' p s = 
    let cok a s' _ = 
      { state = s'
        consumption = Consumed
        result = Ok a }
    let cerr msg s' = 
      { state = s'
        consumption = Consumed
        result = Error msg }
    let eok a s' _ = 
      { state = s'
        consumption = Virgin
        result = Ok a }
    let eerr msg s' = 
      { state = s'
        consumption = Virgin
        result = Error msg }
    unParser p s cok cerr eok eerr
  
  let runParser' p s = 
    let { state = s'; result = result } = runParsec' p s
    match result with
    | Ok a -> (s', Choice2Of2 a)
    | Error msg -> (s', Choice1Of2 msg)
  
  let runParser p s = runParser' p s |> snd
  let inline pMap f p = Parser <| fun s cok cerr eok eerr -> unParser p s (cok << f) cerr (eok << f) eerr
  
  let inline pBind m k = 
    Parser <| fun s cok cerr eok eerr -> 
      let mcok x s' hs = unParser (k x) s' cok cerr (accHints hs cok) (withHints hs cerr)
      let meok x s' hs = unParser (k x) s' cok cerr (accHints hs eok) (withHints hs eerr)
      unParser m s mcok cerr meok eerr
  
  let inline pFail msg = Parser <| fun s _ _ _ eerr -> eerr (newErrorMessage (Message msg) (s.position)) s
  let inline pFailure msgs = Parser <| fun s _ _ _ eerr -> eerr (newErrorMessages msgs s.position) s
  let inline pZero() = Parser <| fun s _ _ _ eerr -> eerr (newErrorUnknown s.position) s
  
  let inline pLabel l p = 
    Parser <| fun s cok cerr eok eerr -> 
      let l' = 
        if Seq.isEmpty l then l
        else "rest of " + l
      let cok' x s' hs = cok x s' <| refreshLastHint hs l
      let eok' x s' hs = eok x s' <| refreshLastHint hs l
      let eerr' err = eerr <| setErrorMessage (Expected l) err
      unParser p s cok' cerr eok' eerr'
  
  let inline pEof() = 
    let parser = 
      Parser <| fun s _ _ eok eerr -> 
        match uncons s.input with
        | None -> eok () s hEmpty
        | Some(x, _) -> eerr (unexpectedErr (showToken x) s.position) s
    pLabel eoi parser
  
  let inline pToken nextpos test = 
    Parser <| fun s cok _ _ eerr -> 
      match uncons s.input with
      | None -> eerr (unexpectedErr "End Of Input" s.position) s
      | Some(c, cs) -> 
        match test c with
        | Choice1Of2 err -> eerr (addErrorMessages err (newErrorUnknown s.position)) s
        | Choice2Of2 a -> 
          let newpos = nextpos s.position c
          let newstate = 
            { input = cs
              position = newpos }
          cok a newstate hEmpty
  
  let inline pTokens nextpos test tts = 
    match uncons tts with
    | None -> Parser <| fun s _ _ eok _ -> eok [] s hEmpty
    | Some(_, _) -> 
      Parser <| fun s cok cerr _ eerr -> 
        let r = showToken << List.rev
        let errExpect x = setErrorMessage (showToken tts |> Expected) (newErrorMessage (Unexpected x) s.position)
        let rec walk ts is rs = 
          match uncons ts with
          | None -> 
            let pos' = nextpos s.position tts
            let s' = 
              { input = rs
                position = pos' }
            cok (List.rev is) s' hEmpty
          | Some(t, ts) -> 
            let errorCont = 
              if Seq.isEmpty is then eerr
              else cerr
            let what = 
              if Seq.isEmpty is then eoi
              else r is
            match uncons rs with
            | None -> errorCont (errExpect what) s
            | Some(x, xs) -> 
              if test t x then walk ts (x :: is) xs
              else errorCont (errExpect <| r (x::is)) s
        walk tts [] s.input
  
  type ParserBuilder() = 
    member x.Bind(func, comp) = pBind func comp
    member x.Return(value) = pReturn value
    member x.ReturnFrom(value) = value
    member x.Zero() = pZero()
  
  let parser = new ParserBuilder()
