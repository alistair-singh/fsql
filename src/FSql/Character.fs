namespace FSql

module Character =
  open Error
  open Position
  open Primitive

  let satisfy f = 
    let testChar (x : char) = 
      if f x then Choice2Of2 x
      else (Choice1Of2 << List.singleton << Unexpected << showToken) x
    pToken updatePosChar testChar
  
  let char' c = satisfy ((=) c) |> pLabel (showToken c)
  let string' (str : seq<char>) = pTokens updatePosString (=) str

  let eqi a b = System.Char.ToLowerInvariant(a) = System.Char.ToLowerInvariant(b) 
  let chari' c = satisfy (eqi c) |> pLabel (showToken c)
  let stringi' (str : seq<char>) = pTokens updatePosString eqi str

