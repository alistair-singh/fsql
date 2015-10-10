module Parser

open Position
open Cursor

type 'a Item = Item of 'a 
               | Error of string 

type CharCursor = Position * char Cursor

type 'a Parser = Parser of (CharCursor -> ('a Item * CharCursor) list)

let parse (Parser p) = p

let item = 
  Parser(function
         | (position, End) -> [Error "end of stream", (position, End)]
         | (position, cursor) -> let char' = value cursor |> Option.get
                                 [Item char', (advance char' position, next cursor)])

let error message = 
  Parser(function
         | charCursor -> [Error message, charCursor])

type ParserBuilder () = 
  member x.Zero () =
    Parser (fun cs -> []) 
  member x.Return a = 
    Parser (fun cs -> [a, cs]) 
  member x.ReturnFrom a = a 
  member x.Bind (p, f) = 
    Parser (fun cs -> List.concat [for (a, cs') in parse p cs -> parse (f a) cs']) 
  static member (++) (p, q) = 
    Parser (fun cs -> (parse p cs) @ (parse q cs)) 

let (++) p q = ParserBuilder.(++) (p, q) 

let (+++) p q = 
  Parser (fun cs -> match (parse (p ++ q) cs) with 
                    | []    -> []
                    | x::xs -> [x]) 

let parser = new ParserBuilder() 

let satisfy p = 
  parser {let! c = item
          match c with
          | Error message -> return! error message
          | Item v ->
            if p (v) then 
              return c
            else
              return! error "does not satisfy condition"}

let single c = satisfy ((=) c)

let rec string = 
  function
  | c::cs -> parser {let! _ = single c
                     let! _ = string cs
                     return Item (c::cs) }
  | [] -> parser {return! error "did not match character"}

