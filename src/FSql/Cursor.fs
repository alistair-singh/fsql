
namespace FSql

module Cursor =

  type 'a Cursor = 
    | Valid of position: int * records: 'a array
    | End

  let ofArray = 
    function
    | [||] -> End
    | n -> Valid (0, n)

  let value = 
    function
    | Valid (index, array) -> Some array.[index]
    | End -> None

  let next = 
    function
    | Valid (index, array) -> 
      if index < (array.Length - 1) then
        Valid (index + 1, array)
      else End
    | End -> End

  let length cursor =
      let rec cLen c n =
          match c with
          | End -> n
          | _ -> cLen (next c) (n + 1)
      cLen cursor 0 

  let nextValue cursor = 
    next cursor 
    |> value

