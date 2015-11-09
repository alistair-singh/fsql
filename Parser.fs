module Parser

open Position
open Cursor

type 'a Item = Item of 'a * Position
               | Error of string * Position

type CharCursor = Position * char Cursor

type 'a Parser = Parser of (CharCursor -> ('a Item * CharCursor) list)

let parse (Parser p) = p

let error message = 
  Parser(function
         | charCursor -> [Error (message, fst charCursor), charCursor])

let item = 
  Parser(function
         | (position, End) -> [Error ("end of stream", position), (position, End)]
         | (position, cursor) -> let char' = value cursor |> Option.get
                                 [Item (char', position), (advance char' position, next cursor)])

let isNotError =
  function
  | (Item (_,_) , _) ->true
  | (Error (_,_) , _) -> false

type ParserBuilder () = 
  member x.Zero () =
    Parser (fun cs -> [Error ("unexpected", fst cs), cs]) 
  member x.Return a = 
    Parser (fun cs -> [Item (a, fst cs), cs]) 
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
          | Error (message, position)-> return! error message
          | Item (v, position) ->
            if p (v) then 
              return c
           }

let single c = satisfy ((=) c)

let rec string = 
  function
  | c::cs -> parser {let! i = single c
                     let position = match i with
                                    | Item (_, pos) -> pos
                                    | Error (_, pos) -> pos
                     let! _ = string cs
                     return c::cs }
  | [] -> parser {return! error "did not match character"}

