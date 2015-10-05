module Parser

open Position
open Cursor

type 'a Item = Item of 'a * Pos
               | Error of string * Pos

type ('a, 'b) Parser = Parser of ('a Cursor -> ('b Item * 'a Cursor) list)

let parse (Parser p) = p

let item = 
  Parser(function
         | End -> []
         | c -> let i = Item (value c |> Option.get, Position.start)
                [i, next c])

let error = 
  Parser(function
         | End -> []
         | c -> [Error ("could not satisfy", Position.start), c])

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
          if p c then 
            return c
          else
            return! error}

let single c = satisfy ((=) c)

let rec string = 
  function
  | c::cs -> parser {let! _ = single c
                     let! _ = string cs
                     return Item (c::cs, Position.start)}
  | [] -> parser {return! error}

