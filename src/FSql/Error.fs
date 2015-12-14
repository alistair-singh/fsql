
namespace FSql

module Error =
  open Position

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

  let badMessage = messageString >> Seq.isEmpty

  type ParseError = { position: Position ; messages : Message list }

  let errorIsUnknown err = Seq.isEmpty err.messages 

  let addErrorMessage m err = 
    let pre = List.filter ((<) m) (err.messages)
    let post = List.filter ((>) m) (err.messages)
    let msgs = pre @ [m] @ post
    { err with messages = msgs }

  let addErrorMessages = List.foldBack addErrorMessage 

  let newErrorUnknown pos = { position = pos; messages = List.empty }

  let newErrorMessages msgs pos = addErrorMessages msgs <| newErrorUnknown pos

  let newErrorMessage msg = newErrorMessages [msg]

  let setErrorMessage msg error =
    let enum = fromEnum msg
    let xs = List.filter (fromEnum >> ((<>) enum)) (error.messages)
    let err = { error with messages = xs }
    if badMessage msg then err else addErrorMessage msg err
  

